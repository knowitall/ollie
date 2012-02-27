package edu.washington.cs.knowitall
package pattern
package extract

import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.pattern.lda.Distributions
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.parse.graph.Graph
import edu.washington.cs.knowitall.tool.parse.pattern.Pattern
import edu.washington.cs.knowitall.tool.parse.pattern.Match
import LdaExtractor.logger
import tool.stem.MorphaStemmer
import org.apache.commons.lang.NotImplementedException

class LdaExtractor private (pattern: Pattern[DependencyNode], private val patternCode: Int, val dist: Distributions) 
extends GeneralExtractor(pattern, dist.patternCount(patternCode), dist.patternCount) {
  import LdaExtractor._
  
  def this(pattern: Pattern[DependencyNode], dist: Distributions) = this(pattern, dist.patternEncoding(pattern.toString), dist)

  override def extract(dgraph: DependencyGraph)(implicit 
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Option[DetailedExtraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {
    val p = dist.patternEncoding(pattern.toString)

    super.extract(dgraph).flatMap { extr =>
      // find relation string that intersects with extraction relation string
      val extrRelationLemmas = extr.rel.text.split(" ").map(MorphaStemmer.instance.lemmatize(_))
      val rels = dist.relationLemmas.filter{case (relation, lemmas) => extrRelationLemmas.forall(exr => lemmas.contains(exr))}.map(_._1)
      if (!rels.isEmpty) logger.debug("matching relstrings: " + rels.mkString(", "))

      // replace the relation
      if (rels.isEmpty) {
	    logger.debug("extraction discarded, no matching relstrings")
	    None
      }
      else {
	    val bestRel = rels.maxBy(rel => dist.prob(dist.relationEncoding(rel))(p))
        val replaced = extr.replaceRelation(bestRel)
        logger.debug("replaced extraction: " + replaced)
        Some(replaced)
      }
    }
  }

  override def confidence(extr: Extraction) = {
    val r = dist.relationEncoding(extr.relText)
    dist.prob(r)(patternCode)
  }
  
  // the confidence is not independent of the extraction
  override def confidence: Option[Double] = None
}

case object LdaExtractor extends PatternExtractorType {
  val logger = LoggerFactory.getLogger(this.getClass)
  def fromLines(lines: Iterator[String]) = throw new NotImplementedException
}