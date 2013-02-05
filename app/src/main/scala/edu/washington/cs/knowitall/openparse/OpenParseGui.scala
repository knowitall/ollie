package edu.washington.cs.knowitall.openparse

import java.awt.Cursor
import java.awt.Dimension
import java.io.File
import java.net.URL
import scala.collection.SortedSet
import scala.io.Source
import scala.swing.Action
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position.Center
import scala.swing.BorderPanel.Position.East
import scala.swing.BorderPanel.Position.North
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.ButtonGroup
import scala.swing.CheckMenuItem
import scala.swing.Component
import scala.swing.Dialog
import scala.swing.FileChooser
import scala.swing.FileChooser.Result
import scala.swing.Label
import scala.swing.ListView
import scala.swing.MainFrame
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Orientation
import scala.swing.RadioMenuItem
import scala.swing.ScrollPane
import scala.swing.Separator
import scala.swing.SimpleSwingApplication
import scala.swing.Slider
import scala.swing.Swing
import scala.swing.TextField
import scala.swing.event.ButtonClicked
import scala.swing.event.SelectionChanged
import scala.swing.event.ValueChanged
import scala.util.control.Exception.catching
import org.apache.batik.swing.JSVGCanvas
import org.apache.batik.swing.svg.JSVGComponent
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.common.Timing.Seconds
import edu.washington.cs.knowitall.common.Timing.time
import edu.washington.cs.knowitall.openparse.eval.Score
import edu.washington.cs.knowitall.openparse.extract.Extraction
import edu.washington.cs.knowitall.openparse.extract.PatternExtractorType
import edu.washington.cs.knowitall.openparse.gui.Dot
import edu.washington.cs.knowitall.openparse.gui.ExtractionEntry
import edu.washington.cs.knowitall.openparse.gui.Parser
import edu.washington.cs.knowitall.openparse.gui.Parser.ParserEnum
import edu.washington.cs.knowitall.openparse.gui.Sentence
import edu.washington.cs.knowitall.tool.parse.DependencyParser
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph.SerializationException
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph.deserialize
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import scopt.OptionParser
import edu.washington.cs.knowitall.ollie.DependencyGraphExtras
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import scala.swing.event.KeyReleased
import edu.washington.cs.knowitall.openparse.eval.GoldSet

object OpenParseGui extends SimpleSwingApplication {
  /** Which parser we are using. */
  var parser: Option[(Parser.ParserEnum, DependencyParser)] = None

  /** Which extractor we are using. */
  var extractor: Option[OpenParse] = None

  /** Which graph is presently being used. */
  var current: Option[DependencyGraph] = None

  /** A gold set of annotations. */
  var gold: Map[String, Boolean] = Map.empty

  /** Which sentences are associated with the slider bar. */
  var sentences: Seq[Sentence] = Seq.empty

  /** The present sentence index. */
  var sentenceIndex = 0;

  /** What to perform on a node click in the graph. */
  var nodeClickEvent: String=>Unit = (nodeText: String) => Unit

  object Settings {
    var rawMatches = false
    var graphvizFile: Option[File] = None // use PATH by default
    var modelUrl: URL = OpenParse.defaultModelUrl
    var sentenceFile: Option[File] = None
    var confidenceThreshold: Double = 0.0
    var goldFile: Option[File] = None

    def configuration = new OpenParse.Configuration(confidenceThreshold = this.confidenceThreshold, collapseGraph = false)
  }

  object Elements {
    val scrollBar = new Slider() {
      orientation = Orientation.Horizontal
      value = 0
      min = 0
      max = 0
      majorTickSpacing = 1
      enabled = false

      def adjust() = {
        if (sentences.size > 1) {
          min = 0
          max = sentences.size - 1
          this.enabled = true
        } else {
          value = 0
          min = 0
          max = 0
          this.enabled = false
        }
      }
    }
  }

  override def main(args: Array[String]) = {
    val parser = new OptionParser("openparse-gui") {
      opt(Some("i"), "input", "<file>", "input file", { v: String => Settings.sentenceFile = Some(new File(v)) })
      opt(Some("m"), "model", "<file>", "model file", { v: String => Settings.modelUrl = new File(v).toURI.toURL })
      doubleOpt(Some("t"), "threshold", "<threshold>", "confident threshold for shown extractions", {
        t: Double => Settings.confidenceThreshold = t
      })
      opt(Some("g"), "gold", "<gold set>", "gold set", { v: String => Settings.goldFile = Some(new File(v)) })
      opt(None, "graphviz", "<file>", "path to graphviz", { v: String => Settings.graphvizFile = Some(new File(v)) })
    }

    if (parser.parse(args)) {
      extractor = Some(OpenParse.fromModelUrl(Settings.modelUrl, Settings.configuration))

      Settings.goldFile.foreach { goldFile =>
        gold = GoldSet.load(goldFile)
      }

      Settings.sentenceFile.foreach { sentenceFile => loadSentences(sentenceFile) }

      super.main(args)
    }
  }

  /** Helper to pop up a dialog to find a file. */
  def choose(default: Option[File] = None): Option[File] = {
    import FileChooser.Result

    val chooser = new FileChooser
    default.map(chooser.selectedFile = _)

    chooser.showOpenDialog(null) match {
      case Result.Approve => Option(chooser.selectedFile)
      case Result.Cancel | Result.Error => None
    }
  }

  def chooseSave(default: Option[File] = None): Option[File] = {
    import FileChooser.Result

    val chooser = new FileChooser
    default.map(chooser.selectedFile = _)

    chooser.showSaveDialog(null) match {
      case Result.Approve => Option(chooser.selectedFile)
      case Result.Cancel | Result.Error => None
    }
  }

  def loadParser(parserType: ParserEnum): Unit =
    parser = Some(Parser.load(parserType))

  def loadParserIfNone(): Unit = parser match {
    case Some(parser) => // nothing
    case None => loadParser(Parser.default)
  }

  def readSentences(file: File): Array[Sentence] = {
    import DependencyGraph._

    // read the sentences
    using (Source.fromFile(file)) { source =>
      val lines = source.getLines.buffered

      val parts = lines.head.split("\t")

      // check if any part is deserializable
      val graphIndex = parts.iterator.indexWhere { column =>
        catching(classOf[SerializationException]).opt {
          deserialize(column)
        }.isDefined
      }

      lines.map { line =>
        val parts = line.split("\t")

        if (graphIndex != -1) {
          Sentence.Graph(deserialize(parts(graphIndex)))
        }
        else {
          Sentence.Text(parts(0))
        }
      }.toArray
    }
  }

  def loadSentences(): Boolean = {
    choose(Settings.sentenceFile) map { file =>
      Settings.sentenceFile = Some(file)
      loadSentences(Settings.sentenceFile.get)
    } match {
      case Some(_) => true
      case None => false
    }
  }

  def loadSentences(file: File) = {
    this.sentences = readSentences(file)

    sentenceIndex = 0
    Elements.scrollBar.value = 0
    Elements.scrollBar.adjust()
  }

  def loadExtractor(extractorType: Option[PatternExtractorType]) = {
    extractor = None
  }

  def top: MainFrame = new MainFrame {
    title = "OpenParse Explorer"
    minimumSize = new Dimension(400, 200)

    // helper methods
    def withCursor(c: java.awt.Cursor)(block: => Unit) =
      try {
        cursor = c
        block
      } finally {
        cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
      }

    def withWaitCursor =
      withCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)) _

    def orReport(block: =>Unit) =
      try {
        block
      }
      catch {
        case e => e.printStackTrace; Dialog.showMessage(message = e.toString)
      }

    def andReport(block: =>Unit) =
      try {
        block
      }
      catch {
        case e => Dialog.showMessage(message = e.toString); throw e
      }

    // important ui elements
    val button = new Button {
      text = "Process"
    }

    val field = new TextField

    val extractionList = new ListView[ExtractionEntry]() {
      listenTo(keys)
      reactions += {
        case KeyPressed(_, Key.Equals, _, _) if this.selection.items.size > 0 =>
          val selection = this.selection.items(0)
          val item = selection.toString.dropWhile(_ != '(')
          gold += item -> true
          this.listData = this.listData.map {
            case item if item == selection => item.annotate(true)
            case other => other
          }
        case KeyReleased(_, Key.Minus, _, _) if this.selection.items.size > 0  =>
          val selection = this.selection.items(0)
          val item = selection.toString.dropWhile(_ != '(')
          gold += item -> false
          this.listData = this.listData.map {
            case item if item == selection => item.annotate(false)
            case other => other
          }
        case KeyReleased(_, Key.Space, _, _) if this.selection.items.size > 0  =>
          val selection = this.selection.items(0)
          val item = selection.toString.dropWhile(_ != '(')
          gold += item -> false
          this.listData = this.listData.map {
            case item if item == selection => item.unannotate
            case other => other
          }
      }
    }

    val label = new Label()

    this.defaultButton = button

    def updateDocument(dgraph: DependencyGraph, dot: String) {
      val svg = Dot.dot2svg(Settings.graphvizFile, dot)
      val doc = Dot.svg2xml(svg, nodeClickEvent)
      canvas.setDocument(doc)
      OpenParseGui.current = Some(dgraph)
    }

    def updateDocument(dgraph: DependencyGraph, extr: ExtractionEntry) {
      val entry = extractionList.selection.items(0)
      val dot = Dot.dotgraph(dgraph, entry)
      updateDocument(dgraph, dot)
    }

    def updateDocument(dgraph: DependencyGraph, nodes: Set[DependencyNode]) {
      val dot = Dot.dotgraph(dgraph, nodes)
      updateDocument(dgraph, dot)
    }

    def updateDocument(dgraph: DependencyGraph) {
      updateDocument(Sentence.Graph(dgraph))
    }

    def updateDocument(sentence: Sentence) {
      field.text = sentence.toString

      val (parseTime, dgraph) = time(parse(sentence))
      val (extractTime, extractions) = time(extract(dgraph))

      extractionList.listData = extractions

      val dot = dgraph.dot("\\n"+dgraph.text)
      updateDocument(dgraph, dot)

      label.text = sentence match {
        case Sentence.Graph(_) => "Input contains dependency graph.  Sentence will not be reparsed."
        case Sentence.Text(_) =>
          parser.map("Parsed using '"+_._2.getClass.getSimpleName+"' in "+Seconds.format(parseTime)+".  ").getOrElse {
            "No parsers selected."
          } +
          "Extracted in "+Seconds.format(extractTime)+"."
      }
    }

    val canvas = new JSVGCanvas
    canvas.setDocumentState(JSVGComponent.ALWAYS_DYNAMIC)

    // menu definition
    def menu = {
      def parserMenuItem(parserType: ParserEnum) = {
        new RadioMenuItem(parserType.toString) {
          this.selected = Parser.default == parserType
          action = Action(this.text) {
            withWaitCursor {
              andReport {
                loadParser(parserType)
                this.selected = true
              }
            }
          }
        }
      }

      def expandMenuItem(display: String, expand: (DependencyGraph,DependencyNode)=>SortedSet[DependencyNode]) = {
        new RadioMenuItem(display) {
          action = Action(display) {
            nodeClickEvent = (nodeText: String) => {
              current.map { dgraph =>
                withWaitCursor {
                  dgraph.nodes.find(_.toFullString.startsWith(nodeText)) match {
                    case Some(node) => updateDocument(dgraph, expand(dgraph, node).toSet)
                    case None => System.err.println("error: node not found: " + nodeText)
                  }
                }
              }
            }
          }
        }
      }

      new MenuBar {
        val parserOptions = Parser.values.map(parserMenuItem(_)).toList

        val expandOptions =
          List(expandMenuItem("Expand Argument", (dgraph: DependencyGraph, node: DependencyNode)=>
            Extraction.expandArgument(dgraph, node, Set())),
          expandMenuItem("Expand Relation", (dgraph: DependencyGraph, node: DependencyNode)=>
            Extraction.expandRelation(dgraph, node, Set()).head.nodes))

        val parserMutex = new ButtonGroup(parserOptions: _*)
        val expandMutex = new ButtonGroup(expandOptions: _*)

        contents += new Menu("File") {
          contents += new MenuItem(Action("Load gold set...") {
            withWaitCursor {
              choose(Settings.goldFile) match {
                case Some(file) =>
                  gold = GoldSet.load(file)
                  extractionList.listData = extractionList.listData.map(entry => entry.copy(correct = gold.get(entry.string)))
                case None =>
              }
            }
          })
          contents += new MenuItem(Action("Save gold set...") {
            withWaitCursor {
              chooseSave(Settings.goldFile) match {
                case Some(file) => GoldSet.save(gold, file)
                case None =>
              }
            }
          })
          contents += new Separator()
          contents += new MenuItem(Action("Search Sentences...") {
            withWaitCursor {
              val query = Dialog.showInput(null, message="Enter the sentence text to search for.", title="Search Sentences", Dialog.Message.Plain, Swing.EmptyIcon, Seq(), "")
              query match {
                case Some(query) =>
                  val index = sentences.indexWhere {
                    case Sentence.Text(s) => s.contains(query)
                    case Sentence.Graph(g) => g.text.contains(query)
                  }

                  if (index >= 0) {
                    sentenceIndex = index
                    Elements.scrollBar.value = index
                    updateDocument(sentences(index))
                  }
                case None => label.text = "No results found."
              }
            }
          })
          contents += new MenuItem(Action("Next Extraction...") {
            withWaitCursor {
              sentences.zipWithIndex.drop(sentenceIndex).foreach {
                case (sentence, index) =>
                  val dgraph = parse(sentence)
                  val extractions = extract(dgraph)

                  if (extractions.size > 0) {
                    sentenceIndex = index
                    Elements.scrollBar.value = index
                    Elements.scrollBar.adjust()
                    updateDocument(dgraph)
                  }
              }
            }
          })
          contents += new Separator()
          contents += new MenuItem(Action("Load Sentences...") {
            withWaitCursor {
              if (loadSentences()) {
                Elements.scrollBar.adjust()
                sentences.headOption.map(updateDocument)
              }
            }
          })
          contents += new MenuItem(Action("Clear Sentences...") {
            sentences = Seq.empty
            Elements.scrollBar.adjust
          })
          contents += new Separator()
          contents += new MenuItem(Action("Exit") { exit() })
        }
        contents += new Menu("Options") {
          contents += new CheckMenuItem("Raw Matches") {
            selected = Settings.rawMatches
            action = Action("Raw Matches") {
              if (this.selected) {
                Settings.rawMatches = true
              } else {
                Settings.rawMatches = false
              }
            }
          }
          contents += new MenuItem(Action("Switch Voice") {
            current.flatMap(dgraph => new DependencyGraphExtras(dgraph).switchVoice.headOption).foreach { dgraph =>
              updateDocument(dgraph)
            }
          })
          contents += new Menu("Parser") {
            contents ++= parserOptions
          }
          contents += new Menu("Click") {
            contents ++= expandOptions
          }
          contents += new Separator()
          contents += new MenuItem(Action("Load default model") {
            extractor = Some(OpenParse.fromModelUrl(OpenParse.defaultModelUrl))
          })
          contents += new MenuItem(Action("Load model...") {
            choose(None) map { file =>
              extractor = Some(OpenParse.fromModelFile(file))
            }
          })
        }
      }
    }

    // user interface
    val ui = new BorderPanel() {
      import BorderPanel.Position._

      val panel = new BoxPanel(Orientation.Vertical) {
        val input = new BorderPanel {
          layout(field) = Center
          layout(button) = East
        }

        contents += input
        contents += Swing.VStrut(10)
        contents += label
        contents += Swing.VStrut(10)
        contents += Elements.scrollBar
        contents += Swing.VStrut(5)
      }
      layout(panel) = North

      val display = new BoxPanel(Orientation.Horizontal) {
        contents += new ScrollPane {
          minimumSize = new Dimension(0, Int.MaxValue)
          maximumSize = new Dimension(200, Int.MaxValue)
          contents = extractionList
        }
        contents += new Component {
          override lazy val peer = canvas
        }
      }
      layout(display) = Center

      listenTo(button)
      listenTo(extractionList.selection)
      listenTo(Elements.scrollBar)

      reactions += {
        case ButtonClicked(b) =>
          withWaitCursor {
            if (field.text.trim.length > 0) {
              val sentence = Sentence(field.text)

              // update state
              if (sentences.isEmpty || sentence != sentences(sentenceIndex)) {
                sentences = sentences.take(sentenceIndex + 1) :+ sentence
              }
              Elements.scrollBar.adjust
              sentenceIndex = Elements.scrollBar.max
              Elements.scrollBar.value = Elements.scrollBar.max

              updateDocument(sentence)
            }
          }

        case SelectionChanged(`extractionList`) => {
          if (extractionList.listData.size > 0) {
            if (extractionList.selection.items.size > 0) {
              withWaitCursor {
                current.map { dgraph =>
                  updateDocument(dgraph, extractionList.selection.items(0))
                }
              }
            }
          }
        }

        case ValueChanged(Elements.scrollBar) => if (sentenceIndex != Elements.scrollBar.value) {
          withWaitCursor {
            if (sentences.indices.contains(Elements.scrollBar.value)) {
              sentenceIndex = Elements.scrollBar.value
              try {
                updateDocument(sentences(sentenceIndex))
              }
              catch {
                case e => e.printStackTrace
              }
            }
          }
        }
      }
    }

    contents = ui
    menuBar = menu
  }

  def parse(sentence: Sentence) = {
    sentence match {
      case Sentence.Text(text) =>
        loadParserIfNone()
        val dgraph = parser.get._2.dependencyGraph(text).collapse
        extractor.map(_.simplifyGraph _) match {
          case Some(f) => f(dgraph)
          case None => dgraph
        }
      case Sentence.Graph(dgraph) =>
        dgraph
    }
  }

  def extract(dgraph: DependencyGraph) = {
    extractor.map { extractor =>
      if (!Settings.rawMatches) {
        val extractions = for {
          (conf, extr) <- extractor.extract(dgraph)
        } yield {
          new ExtractionEntry(conf, extr, parser.map(_._1).getOrElse(Parser.Deserialize), gold.get(extr.toString))
        }

        extractions.sortBy(_.confidence).reverse
      } else {
        val extractions = for {
          ex <- extractor.extractors
          m <- ex.pattern(dgraph.graph)
        } yield {
          ExtractionEntry(None, m, m.nodes.toSet, ex, parser.map(_._1).getOrElse(Parser.Deserialize), m.nodes.iterator.map(_.string).mkString(" "), None)
        }

        extractions.sortBy(_.confidence).reverse

      }
    }.getOrElse(Seq.empty)
  }

  def exit() {
    System.exit(0)
  }
}
