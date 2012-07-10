package edu.washington.cs.knowitall.openparse.extract

import org.slf4j.LoggerFactory

import edu.washington.cs.knowitall.collection.immutable.graph.pattern.{Pattern, Match}
import edu.washington.cs.knowitall.collection.immutable.graph.Graph
import edu.washington.cs.knowitall.tool.parse.graph.{DependencyPattern, DependencyNode, DependencyGraph}
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer.instance

import GeneralExtractor.logger

/** An extractor that is purely specified by a pattern.
  * 
  * @author Michael Schmitz
  */
class GeneralExtractor(pattern: Pattern[DependencyNode], val patternCount: Int, val maxPatternCount: Int) extends PatternExtractor(pattern) {
  import GeneralExtractor._

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

case object GeneralExtractor extends PatternExtractorType {
  val logger = LoggerFactory.getLogger(this.getClass)

  def fromLines(lines: Iterator[String]): List[GeneralExtractor] = {
    val patterns: List[(Pattern[DependencyNode], Int)] = lines.map { line =>
        line.split("\t") match {
          // full information specified
          case Array(pat, count) => (DependencyPattern.deserialize(pat), count.toInt)
          // assume a count of 1 if nothing is specified
          case Array(pat) => logger.warn("warning: pattern has no count: " + pat); (DependencyPattern.deserialize(pat), 1)
          case _ => throw new IllegalArgumentException("line must have one or two columns: " + line)
        }
      }.toList

    val maxCount = patterns.maxBy(_._2)._2
    (for ((p, count) <- patterns) yield {
      new GeneralExtractor(p, count, maxCount)
    }).toList
  }
}
