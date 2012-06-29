package edu.washington.cs.knowitall.openparse

import scopt.OptionParser
import scala.io.Source
import scala.util.control.Exception._
import scala.collection.SortedSet
import org.apache.batik.swing.JSVGCanvas
import org.apache.batik.swing.svg.JSVGComponent
import edu.washington.cs.knowitall.common.Resource._
import edu.washington.cs.knowitall.common.Timing._
import edu.washington.cs.knowitall.openparse.eval.Score
import swing._
import swing.event._
import java.io.File
import uk.co.turingatemyhamster.graphvizs.exec.DotApp
import uk.co.turingatemyhamster.graphvizs.exec.DotLayout
import uk.co.turingatemyhamster.graphvizs.exec.DotOpts
import uk.co.turingatemyhamster.graphvizs.exec.DotFormat
import edu.washington.cs.knowitall.tool.parse._
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.collection.immutable.graph.Graph
import edu.washington.cs.knowitall.openparse.extract._
import edu.washington.cs.knowitall.openparse.OpenParse.implicitBuildExtraction
import edu.washington.cs.knowitall.openparse.OpenParse.implicitValidMatch
import event.Key._
import java.awt.{ Dimension, Graphics2D, Graphics, Image, Rectangle }
import java.awt.{ Color => AWTColor }
import java.awt.event.{ ActionEvent }
import java.awt.Cursor
import javax.swing.{ Timer => SwingTimer, AbstractAction }
import edu.washington.cs.knowitall.openparse.extract.PatternExtractorType
import java.io.IOException
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer

sealed abstract class Sentence
object Sentence {
  case class Text(text: String) extends Sentence {
    override def toString = text
  }
  case class Graph(dgraph: DependencyGraph) extends Sentence {
    override def toString = dgraph.serialize
  }

  def apply(string: String): Sentence = {
    import DependencyGraph._

    catching(classOf[SerializationException]).opt {
      deserialize(string)
    } match {
      case Some(dgraph) => Graph(dgraph)
      case None => Text(string)
    }
  }
}

object Parser extends Enumeration {
  type Parser = Value

  val Stanford = Value("Stanford")
  val MaltL = Value("Malt (Linear)")

  def default = Stanford

  def load(parserType: Parser): DependencyParser = parserType match {
    case Parser.Stanford => new StanfordParser()
    case Parser.MaltL => new MaltParser()
  }
}
import Parser._

object OpenParseGui extends SimpleSwingApplication {
  implicit def implicitValidMatch = OpenParse.validMatch(true)_

  var parser: Option[DependencyParser] = None
  var extractor: Option[OpenParse] = None
  var current: Option[DependencyGraph] = None
  var gold: Map[String, Boolean] = Map.empty
  var sentences: Seq[Sentence] = Seq.empty
  var patterns: Option[List[String]] = None
  var sentenceIndex = 0;

  var nodeClickEvent: String=>Unit = (nodeText: String) => Unit

  case class ExtractionEntry(confidence: Double, extraction: DetailedExtraction) {
    private def goldString = {
      gold.get(extraction.toString) match {
        case Some(true) => "+ "
        case Some(false) => "- "
        case None => ""
      }
    }

    override def toString = goldString + ("%1.4f" format confidence) + ":" + extraction.toString
  }

  object Settings {
    var graphvizFile: Option[File] = None // use PATH by default
    var modelFile: Option[File] = None
    var sentenceFile: Option[File] = None
    var confidenceThreshold: Double = 0.0
    var goldFile: Option[File] = None

    def configuration = new OpenParse.Configuration(confidenceThreshold = this.confidenceThreshold)
  }

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
        }
        else {
          value = 0
          min = 0
          max = 0
          this.enabled = false
        }
      }
    }

  def loadParser(parserType: Parser): Unit =
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
      this.sentences = readSentences(Settings.sentenceFile.get)

      sentenceIndex = 0
      scrollBar.value = 0
      scrollBar.adjust()
    } match {
      case Some(_) => true
      case None => false
    }
  }

  override def main(args: Array[String]) = {
    val parser = new OptionParser("swing") {
      opt(Some("m"), "model", "<file>", "model file", { v: String => Settings.modelFile = Some(new File(v)) })
      doubleOpt(Some("t"), "threshold", "<threshold>", "confident threshold for shown extractions", {
        t: Double => Settings.confidenceThreshold = t
      })
      opt(Some("g"), "gold", "<gold set>", "gold set", { v: String => Settings.goldFile = Some(new File(v)) })
      opt(None, "graphviz", "<file>", "path to graphviz", { v: String => Settings.graphvizFile = Some(new File(v)) })
    }

    if (parser.parse(args)) {
      Settings.modelFile match {
        case Some(file) =>
          extractor = Some(OpenParse.fromModelFile(file, Settings.configuration))
        case None =>
          extractor = Some(OpenParse.fromModelUrl(OpenParse.defaultModelUrl, Settings.configuration))
      }

      Settings.goldFile.foreach { goldFile =>
        gold = Score.loadGoldSet(goldFile)
      }

      super.main(args)
    }
  }

  def choose(default: Option[File] = None): Option[File] = {
    import FileChooser.Result

    val chooser = new FileChooser
    default.map(chooser.selectedFile = _)

    chooser.showOpenDialog(null) match {
      case Result.Approve => Option(chooser.selectedFile)
      case Result.Cancel | Result.Error => None
    }
  }

  def loadExtractor(extractorType: Option[PatternExtractorType]) = {
    patterns = None
    extractor = None
  }

  val dotapp = DotApp(Settings.graphvizFile.getOrElse(new File("/usr/bin/dot")), DotOpts(Some(DotLayout.dot), Some(DotFormat.svg)))
  def dot2svg(dotgraph: String) = {
    import uk.co.turingatemyhamster.graphvizs.exec._
    import uk.co.turingatemyhamster.graphvizs.exec.InputHandler._
    import uk.co.turingatemyhamster.graphvizs.exec.OutputHandler._
    import sys.process.ProcessIO

    val errHandler = OutputHandler.stringOutputHandler
    val inputHandler = StringInputHandler
    val outputHandler = OutputHandler.stringOutputHandler
    val io = new ProcessIO(inputHandler.handle(dotgraph), outputHandler.handle, errHandler.handle, false)

    val process = Settings.graphvizFile match {
      case Some(x) => dotapp.process
      case None => sys.process.Process("dot", dotapp.opts.generate)
    }
    val proc = try (process run io)
    catch {
      case e: IOException =>
        Dialog.showMessage(message = e.getMessage() + ". You may need to install graphviz and add it to the PATH variable, or specify the path to the dot program using the '--graphviz' argument.", messageType = Dialog.Message.Error)
        throw e
    }

    proc.exitValue() match {
      case 0 => outputHandler.value
      case x => sys.error("Dot exited with error code: " + x + " with output:\n" + errHandler.value)
    }
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

    val extractionList = new ListView[ExtractionEntry]()

    val label = new Label()

    this.defaultButton = button

    def updateDocument(dgraph: DependencyGraph, dot: String) {
      val svg = dot2svg(dot)
      val doc = svg2xml(svg)
      canvas.setDocument(doc)
      OpenParseGui.current = Some(dgraph)
    }

    def updateDocument(dgraph: DependencyGraph, extr: ExtractionEntry) {
      val entry = extractionList.selection.items(0)
      val dot = dotgraph(dgraph, entry.extraction)
      updateDocument(dgraph, dot)
    }

    def updateDocument(dgraph: DependencyGraph, nodes: Set[DependencyNode]) {
      val dot = dotgraph(dgraph, nodes)
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
          parser.map("Parsed using '"+_.getClass.getSimpleName+"' in "+Seconds.format(parseTime)+".  ").getOrElse {
            "No parsers selected."
          } +
          "Extracted in "+Seconds.format(extractTime)+"."
      }
    }

    val canvas = new JSVGCanvas
    canvas.setDocumentState(JSVGComponent.ALWAYS_DYNAMIC)

    // menu definition
    def menu = {
      def parserMenuItem(parserType: Parser) = {
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
            Extraction.expandRelation(dgraph, node, Set()).nodes))

        val parserMutex = new ButtonGroup(parserOptions: _*)
        val expandMutex = new ButtonGroup(expandOptions: _*)

        contents += new Menu("File") {
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
                    scrollBar.value = index
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
                    scrollBar.value = index
                    scrollBar.adjust()
                    updateDocument(dgraph)
                  }
              }
            }
          })
          contents += new Separator()
          contents += new MenuItem(Action("Load Sentences...") {
            withWaitCursor {
              if (loadSentences()) {
                scrollBar.adjust()
                sentences.headOption.map(updateDocument)
              }
            }
          })
          contents += new MenuItem(Action("Clear Sentences...") {
            sentences = Seq.empty
            scrollBar.adjust
          })
          contents += new Separator()
          contents += new MenuItem(Action("Exit") { exit() })
        }
        contents += new Menu("Options") {
          contents += new Menu("Parser") {
            contents ++= parserOptions
          }
          contents += new Menu("Click") {
            contents ++= expandOptions
          }
          contents += new Separator()
          contents += new MenuItem(Action("Load default model") {
            OpenParse.fromModelUrl(OpenParse.defaultModelUrl)
          })
          contents += new MenuItem(Action("Load model...") {
            choose(None) map { file =>
              extractor = Some(OpenParse.fromModelFile(file))
            }
          })
          /*
          contents += new MenuItem(Action("Input Pattern...") {
            Dialog.showInput(null, message="Input a pattern.", title="Input Pattern", Dialog.Message.Plain, Swing.EmptyIcon, Seq(), "") map { pat =>
              patterns = Some(List(pat))
              extractorType.map { ex =>
                extractor = Some(new OpenParse(ex.fromLines(List(pat).iterator)))
              }
            }
          })
          contents += new MenuItem(Action("Load Patterns...") {
            choosePatterns().map { px =>
              patterns = Some(px)
              extractorType.map { ex =>
                extractor = Some(new OpenParse(ex.fromLines(px.iterator)))
              }
            }
          })
          */
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
        contents += scrollBar
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
      listenTo(scrollBar)

      reactions += {
        case ButtonClicked(b) =>
          withWaitCursor {
            if (field.text.trim.length > 0) {
              val sentence = Sentence(field.text)

              // update state
              if (sentences.isEmpty || sentence != sentences(sentenceIndex)) {
                sentences = sentences.take(sentenceIndex + 1) :+ sentence
              }
              scrollBar.adjust
              sentenceIndex = scrollBar.max
              scrollBar.value = scrollBar.max

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

        case ValueChanged(`scrollBar`) => if (sentenceIndex != scrollBar.value) {
          withWaitCursor {
            if (sentences.indices.contains(scrollBar.value)) {
              sentenceIndex = scrollBar.value
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

  def dotgraph(dgraph: DependencyGraph, nodes: Set[DependencyNode]) = {
    val nodeStyle = nodes.map((_, "style=filled,color=lightblue"))
    dgraph.dot(dgraph.text, nodeStyle.toMap, Map.empty)
  }

  def dotgraph(dgraph: DependencyGraph, extraction: DetailedExtraction) = {
    val title = "\\n" + dgraph.text + "\\n" + extraction.toString + "\\n" + extraction.`match`.pattern.toStringF((s: String) => if (s.length < 60) s else s.take(20)+"...") +
      (extraction.extractor match { case ex: TemplateExtractor => "\\n" + ex.template case _ => "" })

    // nodes
    val darkNodes = extraction.`match`.nodeGroups
    val lightNodes = extraction.nodes -- darkNodes.map(_._2.node)
    val filledNodes = (lightNodes zip Stream.continually("style=filled,fillcolor=lightgray")) ++
      (darkNodes.map { nodeGroup => val style = "style=filled,fillcolor="+(nodeGroup._1 match {
          case "rel" => "salmon1"
          case "arg1" | "arg2" => "lightblue"
          case "slot0"|"slot1"|"slot2"|"slot3" => "seashell"
          case _ => "yellow"
        })

        (nodeGroup._2.node, style)
      })

    // edges
    val solidEdges = extraction.edges.map(_.edge).toSet

    val nodeStyle = filledNodes
    val edgeStyle = (solidEdges zip Stream.continually("style=filled")) ++
    ((dgraph.graph.edges -- solidEdges) zip Stream.continually("style=dotted,color=gray"))

    dgraph.dot(title, nodeStyle.toMap, edgeStyle.toMap)
  }

  def parse(sentence: Sentence) = {
    (sentence match {
      case Sentence.Text(text) =>
        loadParserIfNone()
        parser.get.dependencyGraph(text)
      case Sentence.Graph(dgraph) =>
        dgraph
    }).simplifyPostags
  }

  def normalize(dgraph: DependencyGraph) = {
    dgraph.simplifyPostags
  }

  def extract(dgraph: DependencyGraph) = {
    val normedDgraph = normalize(dgraph)
    extractor.map { extractor =>
      val extractions = for {
        (conf, extr) <- this.extractor.get.extract(normedDgraph)
      } yield {
        ExtractionEntry(conf, extr)
      }

      extractions.sortBy(_.confidence).reverse
    }.getOrElse(Seq.empty)
  }

  def svg2xml(svgString: String) = {
    import org.apache.batik.dom.svg.SVGDOMImplementation;
    import org.apache.batik.util.XMLResourceDescriptor
    import org.apache.batik.dom.svg.SAXSVGDocumentFactory

    val uri = SVGDOMImplementation.SVG_NAMESPACE_URI;

    val doc = using(new java.io.StringReader(svgString)) { reader =>
      val parser = XMLResourceDescriptor.getXMLParserClassName();
      val f = new SAXSVGDocumentFactory(parser);
      f.createSVGDocument(uri, reader);
    }

    val gs = doc.getElementsByTagNameNS(uri, "g")
    for (i <- 0 until gs.getLength) {
      val g = gs.item(i)
      val attributes = g.getAttributes
      val clazz = attributes.getNamedItem("class").getNodeValue

      if (clazz == "node") {
        val children = g.getChildNodes
        for (j <- 0 until children.getLength) {
          val child = children.item(j)
          if (child.getNodeName == "title") {
            val text = child.getFirstChild.getNodeValue

            import org.w3c.dom.events._
            g.asInstanceOf[EventTarget].addEventListener("click",
              new EventListener() {
                def handleEvent(e: Event) { nodeClickEvent(text) }
              },
              true);
          }
        }
      }
    }

    doc
  }

  def exit() {
    System.exit(0)
  }
}
