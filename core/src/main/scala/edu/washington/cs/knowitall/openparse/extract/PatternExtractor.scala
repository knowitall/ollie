package edu.washington.cs.knowitall.openparse.extract

import java.io.File
import scala.io.Source
import edu.washington.cs.knowitall.collection.immutable.graph.pattern.{Pattern, Match}
import edu.washington.cs.knowitall.collection.immutable.graph.Graph
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.tool.parse.graph.{DependencyNode, DependencyGraph}
import javax.naming.OperationNotSupportedException
import edu.washington.cs.knowitall.collection.immutable.graph.pattern.CaptureNodeMatcher
import edu.washington.cs.knowitall.openparse.ExtractorPattern

/** An superclass for extractors based on patterns.
  *
  * @param  pattern  the pattern to extract
  *
  * @author Michael Schmitz
  */
abstract class PatternExtractor(val pattern: ExtractorPattern) {
  def extract(dgraph: DependencyGraph)(implicit
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Iterable[DetailedExtraction],
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean): Iterable[DetailedExtraction]
  def confidence(extr: Extraction): Double
  def confidence: Option[Double] // independent confidence

  override def toString = pattern.toString

  def tabSerialize: String = throw new OperationNotSupportedException()

  def prepMismatch: Boolean = false
}

object PatternExtractor {
  def tabDeserialize(seq: Seq[String]): (PatternExtractor, Seq[String]) = {
    seq(0).toLowerCase match {
      case "template" => TemplateExtractor.tabDeserialize(seq.drop(1))
    }
  }
}

abstract class PatternExtractorType {
  def fromFile(file: File): Seq[PatternExtractor] = {
    using (Source.fromFile(file, "UTF8")) { source =>
      fromLines(source.getLines)
    }

  }
  def fromLines(lines: Iterator[String]): Seq[PatternExtractor]

  def name = this.getClass.getSimpleName
}

object PatternExtractorType {
  def apply(string: String) = string match {
    case "general" => GeneralExtractor
    case "template" => TemplateExtractor
    case "specific" => SpecificExtractor
    case _ => throw new IllegalArgumentException("unknown extractor: " + string)
  }
}