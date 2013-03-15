package edu.knowitall.openparse.gui

import edu.knowitall.openparse.extract.TemplateExtractor
import edu.knowitall.common.Resource.using
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.parse.graph.DependencyNode
import java.io.IOException
import scala.swing.Dialog
import scala.io.Source
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintWriter
import java.io.File

/** Code pertaining to rendering and converting DOT graphs. */
object Dot {
  def dot2svg(graphvizFile: Option[File], dotgraph: String) = {
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

    val process = graphvizFile match {
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

  def svg2xml(svgString: String, nodeClickEvent: String=>Unit) = {
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

  def dotgraph(dgraph: DependencyGraph, nodes: Set[DependencyNode]) = {
    val nodeStyle = nodes.map((_, "style=filled,color=lightblue"))
    dgraph.dot(dgraph.text, nodeStyle.toMap, Map.empty)
  }

  def dotgraph(dgraph: DependencyGraph, extraction: ExtractionEntry) = {
    def originalNodes(nodes: Iterable[DependencyNode]) = nodes.map { node =>
      dgraph.nodes.find(_.indices == node.indices).get
    }

    val title = "\\n" + dgraph.text + "\\n" + extraction.toString + "\\n" + extraction.`match`.pattern.toStringF((s: String) => if (s.length < 60) s else s.take(20) + "...") +
      (extraction.extractor match { case ex: TemplateExtractor => "\\n" + ex.template case _ => "" })

    // nodes
    val darkNodes = extraction.`match`.nodeGroups
    val lightNodes = originalNodes(extraction.nodes).toSet -- originalNodes(darkNodes.map(_._2.node))
    val filledNodes = (lightNodes zip Stream.continually("style=filled,fillcolor=lightgray")) ++
      (darkNodes.map { nodeGroup =>
        val style = "style=filled,fillcolor=" + (nodeGroup._1 match {
          case "rel" => "salmon1"
          case "arg1" | "arg2" => "lightblue"
          case "slot0" | "slot1" | "slot2" | "slot3" => "seashell"
          case _ => "yellow"
        })

        (nodeGroup._2.node, style)
      })

    // edges
    val solidEdges = extraction.edges.toSet

    val nodeStyle = filledNodes
    val edgeStyle = (solidEdges zip Stream.continually("style=filled")) ++
      ((dgraph.graph.edges.toSet -- solidEdges.toSet) zip Stream.continually("style=dotted,color=gray"))

    dgraph.dot(title, nodeStyle.toMap, edgeStyle.toMap)
  }
}