package edu.washington.cs.knowitall.ollie

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.common.HashCodeHelper

/** OllieExtractionInstance represents an extraction coupled with
  * its source sentence.
  */
class OllieExtractionInstance(
    /** The associated extraction. */
    val extr: OllieExtraction,
    /** The associated sentence. */
    val sent: DependencyGraph) {

  override def equals(that: Any) = that match {
    case that: OllieExtractionInstance => this.extr == that.extr && this.sent == that.sent
    case _ => false
  }
  override def hashCode = HashCodeHelper(extr, sent)

  def tabSerialize: String = {
    val serializedGraph = sent.serialize
    val serializedExtr = extr.tabSerialize
    Seq(serializedGraph, serializedExtr).mkString("\t")
  }
}

class DetailedOllieExtractionInstance(override val extr: DetailedOllieExtraction, sent: DependencyGraph)
extends OllieExtractionInstance(extr, sent)

object OllieExtractionInstance {
  def tabDeserialize(string: String): OllieExtractionInstance = {
    try {
      val (serializedGraph, rest) = string.span(_ != '\t')
      val serializedExtr = rest.drop(1)
      val extr = OllieExtraction.tabDeserialize(serializedExtr).get
      val graph = DependencyGraph.deserialize(serializedGraph)
      new OllieExtractionInstance(extr, graph)
    } catch {
      case e => throw new IllegalArgumentException("Could not tab deserialize: " + string, e)
    }
  }

  val numFinder = "[0-9]+".r
}
