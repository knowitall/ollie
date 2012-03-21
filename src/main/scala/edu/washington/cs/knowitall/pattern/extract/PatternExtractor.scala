package edu.washington.cs.knowitall
package pattern
package extract

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.parse.graph.Graph
import edu.washington.cs.knowitall.tool.parse.pattern.Pattern
import edu.washington.cs.knowitall.tool.parse.pattern.Match
import java.io.File
import scala.io.Source

abstract class PatternExtractor(val pattern: Pattern[DependencyNode]) {
  def extract(dgraph: DependencyGraph)(implicit 
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Option[DetailedExtraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean): Iterable[DetailedExtraction]
  def confidence(extr: Extraction): Double
  def confidence: Option[Double] // independent confidence

  override def toString = pattern.toString
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
    case "lda" => LdaExtractor
    case _ => throw new IllegalArgumentException("unknown extractor: " + string)
  }
}