package edu.washington.cs.knowitall
package pattern
package extract

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.parse.graph.Graph
import edu.washington.cs.knowitall.tool.parse.pattern.Pattern
import edu.washington.cs.knowitall.tool.parse.pattern.Match

abstract class PatternExtractor(val pattern: Pattern[DependencyNode]) {
  def extract(dgraph: DependencyGraph)(implicit 
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Option[DetailedExtraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean): Iterable[DetailedExtraction]
  def confidence(extr: Extraction): Double
  def confidence: Option[Double] // independent confidence

  override def toString = pattern.toString
}
