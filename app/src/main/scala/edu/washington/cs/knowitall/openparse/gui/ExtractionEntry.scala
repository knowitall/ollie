package edu.washington.cs.knowitall.openparse.gui

import edu.washington.cs.knowitall.collection.immutable.graph.pattern.Match
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.openparse.extract.PatternExtractor
import edu.washington.cs.knowitall.openparse.extract.DetailedExtraction

/**
  * A more generic representation of an extraction.
  *
  * This is needed to allow for raw matches, which do
  * not have an arg1, rel, etc.
  */
case class ExtractionEntry(
  confidence: Option[Double],
  `match`: Match[DependencyNode],
  nodes: Set[DependencyNode],
  extractor: PatternExtractor,
  string: String = "") {

  /**
    * Convenient constructor for instantiating from
    * an OpenParse extraction.
    */
  def this(confidence: Double, extraction: DetailedExtraction) = this(Some(confidence), extraction.`match`, extraction.nodes.toSet, extraction.extractor, extraction.toString)

  def edges = `match`.edges

  private def goldString(gold: Map[String, Boolean]) = {
    gold.get(string) match {
      case Some(true) => "+ "
      case Some(false) => "- "
      case None => ""
    }
  }

  override def toString = confidence.map("%1.4f:" format _).getOrElse("") + string
}