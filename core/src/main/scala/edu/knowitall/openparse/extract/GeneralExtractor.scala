package edu.knowitall.openparse.extract

import org.slf4j.LoggerFactory
import edu.knowitall.collection.immutable.graph.pattern.{Pattern, Match}
import edu.knowitall.collection.immutable.graph.Graph
import edu.knowitall.tool.parse.graph.{DependencyPattern, DependencyNode, DependencyGraph}
import edu.knowitall.ollie.Ollie.stemmer
import GeneralExtractor.logger
import edu.knowitall.openparse.ExtractorPattern

/** An extractor that is purely specified by a pattern.
  *
  * @param  pattern  the pattern to extract
  * @param  conf  the confidence of this extractor
  *
  * @author Michael Schmitz
  */
class GeneralExtractor(pattern: ExtractorPattern, val conf: Double) extends PatternExtractor(pattern) {
  import GeneralExtractor._

  def this(pattern: Pattern[DependencyNode], conf: Double) =
    this(new ExtractorPattern(pattern), conf)

  protected def extractWithMatches(dgraph: DependencyGraph)(implicit
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Iterable[DetailedExtraction],
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {

    // apply pattern and keep valid matches
    val matches = pattern(dgraph.graph)
    if (!matches.isEmpty && logger.isDebugEnabled) logger.debug("matches: " + matches.mkString(", "))

    val filtered = matches.filter(validMatch(dgraph.graph))
    if (!filtered.isEmpty && logger.isDebugEnabled) logger.debug("filtered: " + filtered.mkString(", "))

    for (m <- filtered; extr <- buildExtraction(dgraph, m, this)) yield {
      (extr, m)
    }
  }

  override def extract(dgraph: DependencyGraph)(implicit
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Iterable[DetailedExtraction],
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {
    logger.debug("pattern: " + pattern)

    val extractions = this.extractWithMatches(dgraph).map(_._1)
    if (!extractions.isEmpty) logger.debug("extractions: " + extractions.mkString(", "))

    extractions
  }

  override def confidence(extr: Extraction): Double = {
    this.conf
  }

  /** A maximum confidence for any extraction from this extractor.
    * This is used for optimization.  If the minimum confidence is
    * larger than the threshold, we don't need to run this extractor. */
  override def maximumConfidence: Double = this.conf
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

    (for ((p, conf) <- patterns) yield {
      new GeneralExtractor(new ExtractorPattern(p), conf.toDouble)
    }).toList
  }
}
