package edu.washington.cs.knowitall
package pattern

import scala.io.Source
import scala.collection
import scopt.OptionParser
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
  def this(pattern: Pattern[DependencyNode], dist: Distributions) = this(pattern, dist.patternCount(dist.patternEncoding(pattern.toString)), dist.maxPatternCount)

  override def extract(dgraph: DependencyGraph) = {
    // apply pattern and keep valid matches
    val matches = pattern(dgraph.graph)

    val filtered = matches.filter(validMatch(dgraph.graph))

    // convert to an extraction
    filtered.map(m => buildExtraction(dgraph, m.groups))
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
      def cond(e: Graph.Edge[DependencyNode]) = 
        e.label == "det" || e.label == "prep_of" || e.label == "amod" || e.label == "num" || e.label == "nn"
      val inferiors = graph.graph.inferiors(node, cond)
      val indices = inferiors.map(_.indices) reduce (_ ++ _)
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
  def this(pattern: Pattern[DependencyNode], dist: Distributions) = this(pattern, dist.patternEncoding(pattern.toString), dist)

  override def extract(dgraph: DependencyGraph) = {
    val p = dist.patternEncoding(pattern.toString)

    super.extract(dgraph).map { extr =>
      // find relation that maximizes P(p | r)
      val maxr = dist.relationCodes.maxBy(r => dist.prob(r)(p))

      // replace the relation
      extr.replaceRelation(dist.relationDecoding(maxr))
    }
  }

  override def confidence(extr: Extraction) = {
    val r = dist.relationEncoding(extr.rel)
    dist.prob(r)(patternCode)
  }
}

object PatternExtractor {
  val logger = LoggerFactory.getLogger(this.getClass)
  
  /*
  def score(extr: Extraction): Int = {
    // helper methods
    def isProper(node: DependencyNode) = node.postag.equals("NNP") || node.postag.equals("NNPS")
    def isPrep(node: DependencyNode) = node.postag.equals("PRP") || node.postag.equals("PRPS")

    // pimped boolean
    class toInt(b: Boolean) {
      def toInt = if (b) 1 else 0
    }
    implicit def convertBooleanToInt(b: Boolean) = new toInt(b)

    2 + isProper(extr.arg1.text).toInt + isProper(extr.arg2.text).toInt + -isPrep(extr.arg1.text).toInt + -isPrep(extr.arg2).toInt
  }
  */
  
  def confidence(extr: Extraction, count: Int, maxCount: Int): Double = {
    count.toDouble / maxCount.toDouble
  }

  def loadGeneralExtractorsFromFile(patternFilePath: String): List[GeneralPatternExtractor] = {
    val patternSource = Source.fromFile(patternFilePath)
    val patterns: Iterator[(Pattern[DependencyNode], Int)] = try {
      // parse the file
      patternSource.getLines.map { line =>
        line.split("\t") match {
          // full information specified
          case Array(pat, count) => (DependencyPattern.deserialize(pat), count.toInt)
          // assume a count of 1 if nothing is specified
          case Array(pat) => logger.warn("warning: pattern has no count: " + pat); (DependencyPattern.deserialize(pat), 1)
          case _ => throw new IllegalArgumentException("file can't have more than two columns")
        }
      }
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

      opt(Some("p"), "patterns", "<file>", "pattern file", { v: String => patternFilePath = Option(v) })
      opt(None, "lda", "<directory>", "lda directory", { v: String => ldaDirectoryPath = Option(v) })
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
        acc + (pair._1 -> (pair._2 :: acc(pair._1)))
      }
      */
      
      logger.info("performing extractions")
      val sentenceSource = Source.fromFile(parser.sentenceFilePath)
      try {
        for (line <- sentenceSource.getLines) {
          val Array(text, deps) = line.split("\t")
          val nodes = text.split("\\s+").zipWithIndex.map{case (tok, i) => new DependencyNode(tok, null, i)}

          val dependencies = Dependencies.deserialize(deps)
          val dgraph = new DependencyGraph(text, nodes.toList, dependencies).collapseNounGroups.collapseNNPOf
          
          for (
            extractor <- extractors;
            // todo: organize patterns by a reverse-lookup on edges
            // optimization: make sure the dependency graph contains all the edges
            if (extractor.pattern.edgeMatchers.forall(matcher => dependencies.exists(matcher.canMatch(_))));
            extr <- extractor.extract(dgraph) 
          ) {
            val conf = extractor.confidence(extr)
            System.out.println(("%1.6f" format conf) + "\t" + extr + "\t" + extractor.pattern + "\t" + text + "\t" + deps)
          }
        }
      } finally {
        sentenceSource.close
      }
    }
  }
}
