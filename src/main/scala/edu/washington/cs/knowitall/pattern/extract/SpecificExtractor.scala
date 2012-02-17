package edu.washington.cs.knowitall
package pattern
package extract

import scala.Array.canBuildFrom

import edu.washington.cs.knowitall.pattern.lda.Distributions
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.parse.graph.Graph
import edu.washington.cs.knowitall.tool.parse.pattern.Pattern
import edu.washington.cs.knowitall.tool.parse.pattern.Match

import tool.stem.MorphaStemmer

class SpecificExtractor(val relation: String, 
  val relationLemmas: List[String], 
  pattern: Pattern[DependencyNode], patternCount: Int, relationCount: Int) 
extends GeneralExtractor(pattern, patternCount, relationCount) {

  def this(relation: String, 
    pattern: Pattern[DependencyNode], dist: Distributions) =
    this(relation, 
      // todo: hack
      (relation.split(" ").toSet -- PatternExtractor.LEMMA_BLACKLIST).toList,
      pattern, 
      dist.relationByPattern(dist.relationEncoding(relation))._1(dist.patternEncoding(pattern.toString)),
      dist.relationByPattern(dist.relationEncoding(relation))._2)

  override def extract(dgraph: DependencyGraph)(implicit 
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Option[DetailedExtraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {
    val extractions = super.extract(dgraph)
    extractions.withFilter{ extr =>
      val extrRelationLemmas = extr.rel.split(" ").map(MorphaStemmer.instance.lemmatize(_))
      relationLemmas.forall(extrRelationLemmas.contains(_))
    }.map(_.replaceRelation(relation))
  }
}
