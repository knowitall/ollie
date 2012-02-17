package edu.washington.cs.knowitall
package pattern
package extract

import scala.Option.option2Iterable

import org.slf4j.LoggerFactory

import edu.washington.cs.knowitall.pattern.lda.Distributions
import edu.washington.cs.knowitall.tool.parse.graph.Graph
import edu.washington.cs.knowitall.tool.parse.pattern.Match
import edu.washington.cs.knowitall.tool.parse.pattern.Pattern

import GeneralExtractor.logger
import tool.parse.graph.DependencyGraph
import tool.parse.graph.DependencyNode
import tool.parse.graph.Graph
import tool.parse.pattern.Match
import tool.parse.pattern.Pattern

class GeneralExtractor(pattern: Pattern[DependencyNode], val patternCount: Int, val maxPatternCount: Int) extends PatternExtractor(pattern) {
  import GeneralExtractor._
  
  def this(pattern: Pattern[DependencyNode], dist: Distributions) = this(pattern, dist.patternCount(dist.patternEncoding(pattern.toString)), dist.maxPatternCount)

  protected def extractWithMatches(dgraph: DependencyGraph)(implicit
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Option[DetailedExtraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {

    // apply pattern and keep valid matches
    val matches = pattern(dgraph.graph)
    if (!matches.isEmpty) logger.debug("matches: " + matches.mkString(", "))

    val filtered = matches.filter(validMatch(dgraph.graph))
    if (!filtered.isEmpty) logger.debug("filtered: " + filtered.mkString(", "))

    for (m <- filtered; extr <- buildExtraction(dgraph, m, this)) yield {
      (extr, m)
    }
  }

  override def extract(dgraph: DependencyGraph)(implicit 
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Option[DetailedExtraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {
    logger.debug("pattern: " + pattern)
    
    val extractions = this.extractWithMatches(dgraph).map(_._1)
    if (!extractions.isEmpty) logger.debug("extractions: " + extractions.mkString(", "))
    
    extractions
  }

  override def confidence(extr: Extraction): Double = {
    this.confidence.get
  }
  
  override def confidence: Option[Double] = 
    Some(patternCount.toDouble / maxPatternCount.toDouble)
}
object GeneralExtractor {
  val logger = LoggerFactory.getLogger(this.getClass)
}
