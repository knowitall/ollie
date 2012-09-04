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
      val (scoreString, rest) = string.span(_ != '\t')

      val score =
        if (scoreString == 1) true
        else if (scoreString == 0) false
        else throw new IllegalArgumentException("bad score: " + scoreString)
      val inst = OllieExtractionInstance.tabDeserialize(rest.drop(1).dropWhile(_ != '\t').drop(1))
      new ScoredOllieExtractionInstance(score, inst)
    } catch {
      case e => throw new IllegalArgumentException("could not tab deserialize: " + string, e)
    }
  }

  val numFinder = "[0-9]+".r
}
