package edu.washington.cs.knowitall
package pattern

import scala.io.Source
import scala.collection
import scopt.OptionParser
import edu.washington.cs.knowitall.collection.immutable.Interval
import tool.stem.MorphaStemmer
import tool.parse.graph._
import tool.parse.pattern._
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.pattern.lda.Distributions

class Extraction(
    val arg1: String, 
    val rel: String, 
    val arg2: String) {
  override def toString() =
    Iterable(arg1, rel, arg2).mkString("(", ", ", ")")
    
  def replaceRelation(relation: String) = new Extraction(this.arg1, relation, this.arg2)
}

abstract class PatternExtractor(val pattern: Pattern[DependencyNode]) {
  def extract(dgraph: DependencyGraph): Iterable[Extraction]
  def confidence(extr: Extraction): Double

  override def toString = pattern.toString
}

class GeneralPatternExtractor(pattern: Pattern[DependencyNode], val patternCount: Int, val maxPatternCount: Int) extends PatternExtractor(pattern) {
  import GeneralPatternExtractor._
  
  def this(pattern: Pattern[DependencyNode], dist: Distributions) = this(pattern, dist.patternCount(dist.patternEncoding(pattern.toString)), dist.maxPatternCount)

  override def extract(dgraph: DependencyGraph) = {
    logger.debug("pattern: " + pattern)
    
    // apply pattern and keep valid matches
    val matches = pattern(dgraph.graph)
    if (!matches.isEmpty) logger.debug("matches: " + matches.mkString(", "))

    val filtered = matches.filter(validMatch(dgraph.graph))
    if (!filtered.isEmpty) logger.debug("filtered: " + filtered.mkString(", "))

    val extractions = filtered.map(m => buildExtraction(dgraph, m.groups))
    if (!extractions.isEmpty) logger.debug("extractions: " + extractions.mkString(", "))
    
    extractions
  }

  override def confidence(extr: Extraction): Double = {
    patternCount.toDouble / maxPatternCount.toDouble
  }

  private def validMatch(graph: Graph[DependencyNode])(m: Match[DependencyNode]) =
    !m.bipath.nodes.exists { v =>
      // no neg edges
      graph.edges(v).exists(_.label == "neg")
  }


  private def buildExtraction(graph: DependencyGraph, groups: collection.Map[String, DependencyNode]): Extraction = {
    def buildArgument(node: DependencyNode) = {
      def cond(e: Graph.Edge[DependencyNode]) = (e.source.indices borders e.dest.indices) &&
        (e.label == "det" || e.label == "prep_of" || e.label == "amod" || e.label == "num" || e.label == "nn")
      val inferiors = graph.graph.inferiors(node, cond)
      val indices = Interval.union(inferiors.map(_.indices).toSeq)
      // use the original dependencies nodes in case some information
      // was lost.  For example, of is collapsed into the edge prep_of
      val string = graph.nodes.filter(node => node.indices.max >= indices.min && node.indices.max <= indices.max).map(_.text).mkString(" ")
      new DependencyNode(string, node.postag, node.indices)
    }
  
    val rel = groups.find { case (s, dn) => s.equals("rel") }
    val arg1 = groups.find { case (s, dn) => s.equals("arg1") }
    val arg2 = groups.find { case (s, dn) => s.equals("arg2") }
    
    (rel, arg1, arg2) match {
      case (Some((_,rel)), Some((_,arg1)), Some((_,arg2))) => 
        new Extraction(buildArgument(arg1).text, rel.text, buildArgument(arg2).text)
      case _ => throw new IllegalArgumentException("missing group, expected {rel, arg1, arg2}: " + groups)
    }
  }
}
object GeneralPatternExtractor {
  val logger = LoggerFactory.getLogger(this.getClass)
}

class SpecificPatternExtractor(val relation: String, 
  val relationLemmas: List[String], 
  pattern: Pattern[DependencyNode], patternCount: Int, relationCount: Int) 
extends GeneralPatternExtractor(pattern, patternCount, relationCount) {

  def this(relation: String, 
    pattern: Pattern[DependencyNode], dist: Distributions) =
    this(relation, 
      // todo: hack
      (relation.split(" ").toSet -- Set("for", "in", "than", "up", "as", "to", "at", "on", "by", "with", "from", "be", "like", "of")).toList,
      pattern, 
      dist.relationByPattern(dist.relationEncoding(relation))._1(dist.patternEncoding(pattern.toString)),
      dist.relationByPattern(dist.relationEncoding(relation))._2)

  override def extract(dgraph: DependencyGraph) = {
    val extractions = super.extract(dgraph)
    extractions.withFilter{ extr =>
      val extrRelationLemmas = extr.rel.split(" ").map(MorphaStemmer.instance.lemmatize(_))
      relationLemmas.forall(extrRelationLemmas.contains(_))
    }.map(_.replaceRelation(relation))
  }
}

class LdaPatternExtractor private (pattern: Pattern[DependencyNode], private val patternCode: Int, val dist: Distributions) 
extends GeneralPatternExtractor(pattern, dist.patternCount(patternCode), dist.patternCount) {
  import LdaPatternExtractor._
  
  def this(pattern: Pattern[DependencyNode], dist: Distributions) = this(pattern, dist.patternEncoding(pattern.toString), dist)

  override def extract(dgraph: DependencyGraph) = {
    val p = dist.patternEncoding(pattern.toString)

    super.extract(dgraph).flatMap { extr =>
      // find relation string that intersects with extraction relation string
      val extrRelationLemmas = extr.rel.split(" ").map(MorphaStemmer.instance.lemmatize(_))
      val rels = dist.relationEncoding.keys.filter(rel => extrRelationLemmas.forall(exr => rel.contains(exr)))
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
    val r = dist.relationEncoding(extr.rel)
    dist.prob(r)(patternCode)
  }
}
object LdaPatternExtractor {
  val logger = LoggerFactory.getLogger(this.getClass)
}

object PatternExtractor {
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def confidence(extr: Extraction, count: Int, maxCount: Int): Double = {
    count.toDouble / maxCount.toDouble
  }

  def loadGeneralExtractorsFromFile(patternFilePath: String): List[GeneralPatternExtractor] = {
    val patternSource = Source.fromFile(patternFilePath)
    val patterns: List[(Pattern[DependencyNode], Int)] = try {
      // parse the file
      patternSource.getLines.map { line =>
        line.split("\t") match {
          // full information specified
          case Array(pat, count) => (DependencyPattern.deserialize(pat), count.toInt)
          // assume a count of 1 if nothing is specified
          case Array(pat) => logger.warn("warning: pattern has no count: " + pat); (DependencyPattern.deserialize(pat), 1)
          case _ => throw new IllegalArgumentException("file can't have more than two columns")
        }
      }.toList
    } finally {
      patternSource.close
    }

    val maxCount = patterns.maxBy(_._2)._2
    (for ((p, count) <- patterns) yield {
      new GeneralPatternExtractor(p, count, maxCount)
    }).toList
  }
  
  def loadLdaExtractorsFromDistributions(dist: Distributions): List[LdaPatternExtractor] = {
    (for (p <- dist.patternCodes) yield {
      new LdaPatternExtractor(DependencyPattern.deserialize(dist.patternDecoding(p)), dist)
    }).toList
  }

  def loadGeneralExtractorsFromDistributions(dist: Distributions): List[GeneralPatternExtractor] = {
    (for (p <- dist.patternCodes) yield {
      new GeneralPatternExtractor(DependencyPattern.deserialize(dist.patternDecoding(p)), dist)
    }).toList
  }

  def loadSpecificExtractorsFromDistributions(dist: Distributions): List[GeneralPatternExtractor] = {
    (for (p <- dist.patternCodes; 
      val pattern = DependencyPattern.deserialize(dist.patternDecoding(p));
      r <- dist.relationsForPattern(p)) yield {
      new SpecificPatternExtractor(dist.relationDecoding(r),
        pattern, 
        dist)
    }).toList
  }
  
  def main(args: Array[String]) {
    val parser = new OptionParser("applypat") {
      var patternFilePath: Option[String] = None
      var ldaDirectoryPath: Option[String] = None
      var sentenceFilePath: String = null
      var extractorType: String = null
      var duplicates: Boolean = false

      opt(Some("p"), "patterns", "<file>", "pattern file", { v: String => patternFilePath = Option(v) })
      opt(None, "lda", "<directory>", "lda directory", { v: String => ldaDirectoryPath = Option(v) })
      booleanOpt("d", "duplicates", "<boolean>", { v: Boolean => duplicates = v })
      arg("type", "type of extractor", { v: String => extractorType = v })
      arg("sentences", "sentence file", { v: String => sentenceFilePath = v })
    }
    
    if (parser.parse(args)) {
      // optionally load the distributions
      val distributions = parser.ldaDirectoryPath.map {
        logger.info("loading distributions")
        Distributions.fromDirectory(_)
      }
      
      logger.info("reading patterns")
      // sort by inverse count so frequent patterns appear first 
      val extractors = ((parser.extractorType, distributions, parser.patternFilePath) match {
        case (_, Some(_), Some(_)) => throw new IllegalArgumentException
        case ("lda", Some(distributions), None) => loadLdaExtractorsFromDistributions(distributions)
        case ("general", Some(distributions), None) => loadGeneralExtractorsFromDistributions(distributions)
        case ("specific", Some(distributions), None) => loadSpecificExtractorsFromDistributions(distributions)
        case ("general", None, Some(patternFilePath)) => loadGeneralExtractorsFromFile(patternFilePath)
        case _ => throw new IllegalArgumentException
      }).toList
      
      /*
      logger.info("building reverse lookup")
      val reverseLookup = (for (extractor <- extractors; edge <- extractor.pattern.edgeMatchers.collect{case m: DependencyEdgeMatcher => m}) yield {
        (edge.label, extractor)
      }).foldLeft(Map[String, List[PatternExtractor]]().withDefaultValue(List())) { (acc, pair) => 
        acc + (pair._1 -> (pair._2 :: acc(pair._1))))
      }
      */
      
      case class Result(conf: Double, extr: Extraction, rest: String) extends Ordered[Result] {
        override def toString = {
          ("%1.6f" format conf) + "\t" + extr + "\t" + rest
        }
        
        override def compare(that: Result) = this.conf.compare(that.conf)
      }
      
      logger.info("performing extractions")
      val sentenceSource = Source.fromFile(parser.sentenceFilePath)
      try {
        for (line <- sentenceSource.getLines) {
          val parts = line.split("\t")
          require(parts.length <= 2, "each line in sentence file must have no more than two columns: " + line)

          val dependencyString = parts.last
          val dependencies = Dependencies.deserialize(dependencyString)
          val text = if (parts.length > 1) Some(parts(0)) else None

          val dgraph = DependencyGraph(text, dependencies).collapseNounGroups.collapseNNPOf
          if (text.isDefined) logger.debug("text: " + text.get)
          logger.debug("graph: " + Dependencies.serialize(dgraph.dependencies))
          
          val results = for (
            extractor <- extractors;
            // todo: organize patterns by a reverse-lookup on edges
            // optimization: make sure the dependency graph contains all the edges
            if (extractor.pattern.edgeMatchers.forall(matcher => dependencies.exists(matcher.canMatch(_))));
            extr <- extractor.extract(dgraph) 
          ) yield {
            val conf = extractor.confidence(extr)
            Result(conf, extr, extractor.pattern + "\t" + ("" /: text)((_, s) => s + "\t") + dependencyString)
          }
          
          if (parser.duplicates) {
            for (result <- results.sorted(Ordering[Result].reverse)) {
              println(result)
            }
          }
          else {
            val maxes = for (results <- results.groupBy(_.extr)) yield (results._2.max)
            for (result <- maxes.toSeq.sorted(Ordering[Result].reverse)) {
              println(result)
            }
          }
        }
      } finally {
        sentenceSource.close
      }
    }
  }
}
