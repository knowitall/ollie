package edu.knowitall.ollie

import edu.knowitall.common.HashCodeHelper
import edu.knowitall.openparse.extract.PatternExtractor
import edu.knowitall.tool.parse.graph.DependencyGraph
import scala.util.matching.Regex

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

  private val passivePatternRegex = new Regex("""^\{arg1:?\w*\} <nsubjpass<.*""")
  /** Report if this extraction is an passive construction.
    * This is a crude measure so false should not be taken to mean
    * that it is not active.
    *
    * An extraction is passive if it has a valid active formulation.
    */
  def passive: Boolean =
    passivePatternRegex.pattern.matcher(pat.pattern.serialize).matches() && (extr.rel.text.endsWith(" by"))

  private val activePatternRegex = new Regex("""^\{arg1:?\w*\} <nsubj<.*>dobj> \{arg2:?\w*\}""")
  /** Report if this extraction is an active construction.
    * This is a crude measure so false should not be taken to mean
    * that it is not active.
    *
    * An extraction is active if it has a valid passive formulation
    * by swapping the arguments and modifying the relation (adding "be"
    * and "by").
    */
  def active: Boolean =
    activePatternRegex.pattern.matcher(pat.pattern.serialize).matches()

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
