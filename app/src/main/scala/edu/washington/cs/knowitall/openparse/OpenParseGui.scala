package edu.washington.cs.knowitall.openparse

import scopt.OptionParser
import scala.io.Source
import scala.util.control.Exception._
import org.apache.batik.swing.JSVGCanvas
import org.apache.batik.swing.svg.JSVGComponent
import edu.washington.cs.knowitall.common.Resource._
import edu.washington.cs.knowitall.common.Timing._
import edu.washington.cs.knowitall.openparse.eval.Score
import swing._
import swing.event._
import java.io.File
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
import edu.washington.cs.knowitall.collection.immutable.graph.DirectedEdge
import edu.washington.cs.knowitall.collection.immutable.graph.pattern.Match
import java.net.URL
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
import scala.collection.SortedSet

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

  def default = MaltL

  def load(parserType: Parser): DependencyParser = parserType match {
    case Parser.Stanford => throw new IllegalArgumentException("Stanford parser not supported due to licensing restrictions.")
    case Parser.MaltL => new MaltParser()
  }
}
import Parser._

object OpenParseGui extends SimpleSwingApplication {
  var parser: Option[DependencyParser] = None
  var extractor: Option[OpenParse] = None
  var current: Option[DependencyGraph] = None
  var gold: Map[String, Boolean] = Map.empty
  var sentences: Seq[Sentence] = Seq.empty
  var sentenceIndex = 0;

  var nodeClickEvent: String=>Unit = (nodeText: String) => Unit

  case class ExtractionEntry(confidence: Option[Double], `match`: Match[DependencyNode], nodes: Set[DependencyNode], extractor: PatternExtractor, string: String = "") {
    def this(confidence: Double, extraction: DetailedExtraction) = this(Some(confidence), extraction.`match`, extraction.nodes.toSet, extraction.extractor, extraction.toString)

    def edges = `match`.edges

    private def goldString = {
      gold.get(string) match {
        case Some(true) => "+ "
        case Some(false) => "- "
        case None => ""
      }
    }

    override def toString = goldString + confidence.map("%1.4f:" format _).getOrElse("") + string
  }

  object Settings {
    var rawMatches = false
    var graphvizFile: Option[File] = None // use PATH by default
    var modelUrl: URL = OpenParse.defaultModelUrl
    var sentenceFile: Option[File] = None
    var confidenceThreshold: Double = 0.0
    var goldFile: Option[File] = None

    def configuration = new OpenParse.Configuration(confidenceThreshold = this.confidenceThreshold, collapseGraph = false)
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
      loadSentences(Settings.sentenceFile.get)
    } match {
      case Some(_) => true
      case None => false
    }
  }

  def loadSentences(file: File) = {
    this.sentences = readSentences(file)

    sentenceIndex = 0
    scrollBar.value = 0
    scrollBar.adjust()
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
        gold = Score.loadGoldSet(goldFile)
      }

      Settings.sentenceFile.foreach { sentenceFile => loadSentences(sentenceFile) }

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
    extractor = None
  }

  def dot2svg(dotgraph: String) = {
    import sys.process.ProcessIO

    trait InputHandler[A] {
      def handle(a: A)(input: OutputStream)
    }

    trait OutputHandler[A] {
      def handle(output: InputStream)
      def value: A
    }

    val errHandler = new OutputHandler[String] {
      var value: String = null

      def handle(out: InputStream) {
        value = Source.fromInputStream(out).mkString
        out.close()
      }
    }

    val inputHandler = new InputHandler[String] {
      def handle(a: String)(os: OutputStream) {
        val pw = new PrintWriter(os)
        pw write a
        pw.close()
      }
    }

    val outputHandler = new OutputHandler[String] {
      var value: String = null

      def handle(out: InputStream) {
        value = Source.fromInputStream(out).mkString
        out.close()
      }
    }
    val io = new ProcessIO(inputHandler.handle(dotgraph), outputHandler.handle, errHandler.handle, false)

    val process = Settings.graphvizFile match {
      case Some(file) => sys.process.Process(file.getAbsolutePath, Seq("-T", "svg"))
      case None => sys.process.Process("dot", Seq("-T", "svg"))
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
      val dot = dotgraph(dgraph, entry)
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

  def dotgraph(dgraph: DependencyGraph, extraction: ExtractionEntry) = {
    def originalNodes(nodes: Iterable[DependencyNode]) = nodes.map { node =>
      dgraph.nodes.find(_.indices == node.indices).get
    }

    val title = "\\n" + dgraph.text + "\\n" + extraction.toString + "\\n" + extraction.`match`.pattern.toStringF((s: String) => if (s.length < 60) s else s.take(20)+"...") +
      (extraction.extractor match { case ex: TemplateExtractor => "\\n" + ex.template case _ => "" })

    // nodes
    val darkNodes = extraction.`match`.nodeGroups
    val lightNodes = originalNodes(extraction.nodes).toSet -- originalNodes(darkNodes.map(_._2.node))
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
    val solidEdges = extraction.edges.toSet

    val nodeStyle = filledNodes
    val edgeStyle = (solidEdges zip Stream.continually("style=filled")) ++
    ((dgraph.graph.edges -- solidEdges) zip Stream.continually("style=dotted,color=gray"))

    dgraph.dot(title, nodeStyle.toMap, edgeStyle.toMap)
  }

  def parse(sentence: Sentence) = {
    sentence match {
      case Sentence.Text(text) =>
        loadParserIfNone()
        val dgraph = parser.get.dependencyGraph(text).collapse
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
          new ExtractionEntry(conf, extr)
        }

        extractions.sortBy(_.confidence).reverse
      } else {
        val extractions = for {
          ex <- extractor.extractors
          m <- ex.pattern(dgraph.graph)
        } yield {
          ExtractionEntry(None, m, m.nodes.toSet, ex, m.nodes.iterator.map(_.string).mkString(" "))
        }

        extractions.sortBy(_.confidence).reverse

      }
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
