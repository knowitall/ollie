package edu.washington.cs.knowitall.openparse.extract

import scala.Array.canBuildFrom
import edu.washington.cs.knowitall.collection.immutable.graph.pattern.{Pattern, Match}
import edu.washington.cs.knowitall.collection.immutable.graph.Graph
import edu.washington.cs.knowitall.tool.parse.graph.{DependencyNode, DependencyGraph}
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer
import edu.washington.cs.knowitall.openparse.ExtractorPattern

/** An extractor that is specified only with a pattern
  * but only works for specific relation lemmas.
  *
  * @param  relation  the resulting relation string
  * @param  relationLemmas  the acceptible matched lemmas
  * @param  pattern  the pattern to extract
  * @param  conf  the confidence of this extractor
  *
  * @author Michael Schmitz
  */
class SpecificExtractor(val relation: String,
  val relationLemmas: List[String],
  pattern: ExtractorPattern, conf: Double)
extends GeneralExtractor(pattern, conf) {

  def this(relation: String, relationLemmas: List[String], pattern: Pattern[DependencyNode], conf: Double) =
    this(relation, relationLemmas, new ExtractorPattern(pattern), conf)

  override def extract(dgraph: DependencyGraph)(implicit
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Iterable[DetailedExtraction],
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