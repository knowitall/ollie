package edu.washington.cs.knowitall.ollie

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.common.HashCodeHelper

/** OllieExtractionInstance represents a boolean score coupled with
  * an extraciton instance.
  */
case class ScoredOllieExtractionInstance(
    val score: Boolean,
    val inst: OllieExtractionInstance) {

  override def toString = score + ":" + inst.extr

  def tabSerialize: String = {
    Iterable(if (score) 1 else 0, inst.extr.toString, inst.tabSerialize).mkString("\t")
  }
}

object ScoredOllieExtractionInstance {
  def tabDeserialize(string: String): ScoredOllieExtractionInstance = {
    try {
      val Array(scoreString, _, rest @ _*) = string.split('\t')

      val score =
        if (scoreString == "1") true
        else if (scoreString == "0") false
        else throw new IllegalArgumentException("bad score: " + scoreString)
      val (inst, r2) = OllieExtractionInstance.tabDeserialize(rest)

      require(r2.isEmpty)

      new ScoredOllieExtractionInstance(score, inst)
    } catch {
      case e => throw new IllegalArgumentException("could not tab deserialize: " + string, e)
    }
  }

  val numFinder = "[0-9]+".r
}
