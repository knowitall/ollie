package edu.washington.cs.knowitall.openparse.extract

import scala.Array.canBuildFrom

import edu.washington.cs.knowitall.collection.immutable.graph.pattern.{Pattern, Match}
import edu.washington.cs.knowitall.collection.immutable.graph.Graph
import edu.washington.cs.knowitall.tool.parse.graph.{DependencyNode, DependencyGraph}
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer

/** An extractor that is specified only with a pattern 
  * but only works for specific relation lemmas.
  * 
  * @author Michael Schmitz
  */
class SpecificExtractor(val relation: String, 
  val relationLemmas: List[String], 
  pattern: Pattern[DependencyNode], conf: Double) 
extends GeneralExtractor(pattern, conf) {

  override def extract(dgraph: DependencyGraph)(implicit 
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Option[DetailedExtraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {
    val extractions = super.extract(dgraph)
    extractions.withFilter{ extr =>
      val extrRelationLemmas = extr.rel.text.split(" ").map(MorphaStemmer.instance.lemmatize(_))
      relationLemmas.forall(extrRelationLemmas.contains(_))
    }.map(_.replaceRelation(relation))
  }
}

case object SpecificExtractor extends PatternExtractorType {
  def fromLines(lines: Iterator[String]) = throw new UnsupportedOperationException
}