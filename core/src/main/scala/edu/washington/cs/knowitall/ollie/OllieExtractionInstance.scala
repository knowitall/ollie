package edu.washington.cs.knowitall.ollie

import edu.washington.cs.knowitall.common.HashCodeHelper
import edu.washington.cs.knowitall.openparse.extract.PatternExtractor
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

/** OllieExtractionInstance represents an extraction coupled with
  * its source sentence.
  */
class OllieExtractionInstance(
    /** The associated extraction. */
    val extr: OllieExtraction,
    /** The associated sentence. */
    val sent: DependencyGraph,
    /** The extractor used. */
    val pat: PatternExtractor) {

  override def equals(that: Any) = that match {
    case that: OllieExtractionInstance => this.extr == that.extr && this.sent == that.sent
    case _ => false
  }
  override def hashCode = HashCodeHelper(extr, sent)

  def extraction = extr
  def sentence = sent
  def pattern = pat

  def tabSerialize: String = {
    val serializedGraph = sent.serialize
    val serializedExtr = extr.tabSerialize
    Seq(serializedGraph, pat.tabSerialize, serializedExtr).mkString("\t")
  }
}

object OllieExtractionInstance {
  def tabDeserialize(string: String): OllieExtractionInstance = {
    val array = string.split('\t')

    val (extr, rest) = tabDeserialize(array)
    require(rest.isEmpty)

    extr
  }

  def tabDeserialize(array: Seq[String]): (OllieExtractionInstance, Seq[String]) = {
    try {
      val Seq(serializedGraph, r0 @ _*) = array

      val graph = DependencyGraph.deserialize(serializedGraph)
      val (pat, r1) = PatternExtractor.tabDeserialize(r0)
      val (extr, r2) = OllieExtraction.tabDeserialize(r1)

      (new OllieExtractionInstance(extr, graph, pat), r2)
    } catch {
      case e => throw new IllegalArgumentException("Could not tab deserialize: " + array.mkString("\t"), e)
    }
  }

  val numFinder = "[0-9]+".r
}
