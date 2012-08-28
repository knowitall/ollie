package edu.washington.cs.knowitall.ollie

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

/** OllieExtractionInstance represents an extraction coupled with
  * its source sentence.
  */
class OllieExtractionInstance(
    /** The associated extraction. */
    val extr: OllieExtraction,
    /** The associated sentence. */
    val sent: DependencyGraph) {

  def tabDelimited: String = {
    val serializedGraph = sent.serialize
    val serializedExtr = extr.serialize
    Seq(serializedExtr, serializedGraph).mkString("_&&&_")
  }
}

class DetailedOllieExtractionInstance(override val extr: DetailedOllieExtraction, sent: DependencyGraph)
extends OllieExtractionInstance(extr, sent)

object OllieExtractionInstance {
  def deserialize(string: String): Option[OllieExtractionInstance] = {
    def error = { System.err.println("Couldn't deserialize %s".format(string)); None }
    try {
      val Array(serializedExtr, serializedGraph) = string.split("_&&&_")
      val extr = OllieExtraction.deserialize(serializedExtr).get
      val graph = DependencyGraph.deserialize(serializedGraph)
      Some(new OllieExtractionInstance(extr, graph))
    } catch {
      case e => { e.printStackTrace; error }
    }
  }

  val numFinder = "[0-9]+".r
}
