package edu.washington.cs.knowitall
package pattern

import scala.io.Source
import scala.util.matching.Regex
import scala.collection
import scopt.OptionParser
import edu.washington.cs.knowitall.collection.immutable.Interval
import tool.stem.MorphaStemmer
import tool.parse.graph._
import tool.parse.pattern._
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.pattern.lda.Distributions
import edu.washington.cs.knowitall.util.DefaultObjects
import edu.washington.cs.knowitall.extractor.ReVerbExtractor
import edu.washington.cs.knowitall.nlp.OpenNlpSentenceChunker
import edu.washington.cs.knowitall.normalization.RelationString

class Extraction(
    val arg1: String, 
    val rel: String, 
    val relLemmas: Option[Set[String]], 
    val arg2: String) {

  def this(arg1: String, rel: String, arg2: String) = this(arg1, rel, None, arg2)

  override def equals(that: Any) = that match {
    case that: Extraction => (that canEqual this) && that.arg1 == this.arg1 && that.rel == this.rel && that.arg2 == this.arg2
    case _ => false
  }
  def canEqual(that: Any) = that.isInstanceOf[Extraction]
  override def hashCode = arg1.hashCode + 39 * (rel.hashCode + 39 * arg2.hashCode)

  override def toString() =
    Iterable(arg1, rel, arg2).mkString("(", ", ", ")")
    
  def replaceRelation(relation: String) = new Extraction(this.arg1, relation, relLemmas, this.arg2)
  def softMatch(that: Extraction) = 
    (that.arg1.contains(this.arg1) || this.arg1.contains(that.arg1)) &&
    this.relLemmas == that.relLemmas &&
    (that.arg2.contains(this.arg2) || this.arg2.contains(that.arg2))
}

case class Template(template: String) {
  import Template._
  def apply(extr: Extraction, m: Match[DependencyNode]) = {
    val rel = group.replaceAllIn(template, (gm: Regex.Match) => m.groups(gm.group(1)).text)

    extr.replaceRelation(rel)
  }
}
object Template {
  val group = """\{(.*?)}""".r
  def deserialize(string: String) = Template(string)
}

abstract class PatternExtractor(val pattern: Pattern[DependencyNode]) {
  def extract(dgraph: DependencyGraph)(implicit 
    buildExtraction: (DependencyGraph, Match[DependencyNode])=>Option[Extraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean): Iterable[Extraction]
  def confidence(extr: Extraction): Double

  override def toString = pattern.toString
}

class GeneralPatternExtractor(pattern: Pattern[DependencyNode], val patternCount: Int, val maxPatternCount: Int) extends PatternExtractor(pattern) {
  import GeneralPatternExtractor._
  
  def this(pattern: Pattern[DependencyNode], dist: Distributions) = this(pattern, dist.patternCount(dist.patternEncoding(pattern.toString)), dist.maxPatternCount)

  protected def extractWithMatches(dgraph: DependencyGraph)(implicit
    buildExtraction: (DependencyGraph, Match[DependencyNode])=>Option[Extraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {

    // apply pattern and keep valid matches
    val matches = pattern(dgraph.graph)
    if (!matches.isEmpty) logger.debug("matches: " + matches.mkString(", "))

    val filtered = matches.filter(validMatch(dgraph.graph))
    if (!filtered.isEmpty) logger.debug("filtered: " + filtered.mkString(", "))

    for (m <- filtered; extr <- buildExtraction(dgraph, m)) yield {
      (extr, m)
    }
  }

  override def extract(dgraph: DependencyGraph)(implicit 
    buildExtraction: (DependencyGraph, Match[DependencyNode])=>Option[Extraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {
    logger.debug("pattern: " + pattern)
    
    val extractions = this.extractWithMatches(dgraph).map(_._1)
    if (!extractions.isEmpty) logger.debug("extractions: " + extractions.mkString(", "))
    
    extractions
  }

  override def confidence(extr: Extraction): Double = {
    patternCount.toDouble / maxPatternCount.toDouble
  }
}
object GeneralPatternExtractor {
  val logger = LoggerFactory.getLogger(this.getClass)
}

class TemplatePatternExtractor(val template: Template, pattern: Pattern[DependencyNode], patternCount: Int, maxPatternCount: Int) 
extends GeneralPatternExtractor(pattern, patternCount, maxPatternCount) {
  override def extract(dgraph: DependencyGraph)(implicit
    buildExtraction: (DependencyGraph, Match[DependencyNode])=>Option[Extraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {

    val extractions = super.extractWithMatches(dgraph)

    extractions.map{ case (extr, m) => template(extr, m) }
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
      (relation.split(" ").toSet -- PatternExtractor.LEMMA_BLACKLIST).toList,
      pattern, 
      dist.relationByPattern(dist.relationEncoding(relation))._1(dist.patternEncoding(pattern.toString)),
      dist.relationByPattern(dist.relationEncoding(relation))._2)

  override def extract(dgraph: DependencyGraph)(implicit 
    buildExtraction: (DependencyGraph, Match[DependencyNode])=>Option[Extraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {
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

  override def extract(dgraph: DependencyGraph)(implicit 
    buildExtraction: (DependencyGraph, Match[DependencyNode])=>Option[Extraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {
    val p = dist.patternEncoding(pattern.toString)

    super.extract(dgraph).flatMap { extr =>
      // find relation string that intersects with extraction relation string
      val extrRelationLemmas = extr.rel.split(" ").map(MorphaStemmer.instance.lemmatize(_))
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
    val r = dist.relationEncoding(extr.rel)
    dist.prob(r)(patternCode)
  }
}
object LdaPatternExtractor {
  val logger = LoggerFactory.getLogger(this.getClass)
}

object PatternExtractor {
  val LEMMA_BLACKLIST = Set("for", "in", "than", "up", "as", "to", "at", "on", "by", "with", "from", "be", "like", "of")
  val VALID_ARG_POSTAG = Set("NN", "NNS", "NNP", "NNPS", "JJ", "JJS")
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def confidence(extr: Extraction, count: Int, maxCount: Int): Double = {
    count.toDouble / maxCount.toDouble
  }
  
  private def validMatch(restrictArguments: Boolean)(graph: Graph[DependencyNode])(m: Match[DependencyNode]) = {
    // no neighboring neg edges
    !m.bipath.nodes.exists { v =>
      graph.edges(v).exists(_.label == "neg")
	} && 
	(!restrictArguments || (VALID_ARG_POSTAG.contains(m.nodeGroups("arg1").node.postag) && VALID_ARG_POSTAG.contains(m.nodeGroups("arg2").node.postag)))
  }

  private def buildExtraction(expandArgument: Boolean)(graph: DependencyGraph, m: Match[DependencyNode]): Option[Extraction] = {
    val groups = m.nodeGroups
  
    val rel = groups.get("rel").map(_.node) getOrElse(throw new IllegalArgumentException("no rel: " + m))
    val arg1 = groups.get("arg1").map(_.node) getOrElse(throw new IllegalArgumentException("no arg1: " + m)) 
    val arg2 = groups.get("arg2").map(_.node) getOrElse(throw new IllegalArgumentException("no arg2: " + m))
    
    def expand(node: DependencyNode) = {
      // don't restrict to adjacent (by interval) because prep_of, etc.
      // remove some nodes that we want to expand across.  In the end,
      // we get the span over the inferiors.
      def cond(e: Graph.Edge[DependencyNode]) = 
        (e.label == "det" || e.label == "prep_of" || e.label == "amod" || e.label == "num" || e.label == "nn" || e.label == "poss")
      val inferiors = graph.graph.inferiors(node, cond).toList.sortBy(_.indices)

      val lefts = inferiors.takeWhile(_ != node).reverse
      val rights = inferiors.dropWhile(_ != node).drop(1)

      val indices = Interval.span(node.indices :: lefts.takeWhile(_ != rel).map(_.indices) ++ rights.takeWhile(_ != rel).map(_.indices))

      // use the original dependencies nodes in case some information
      // was lost.  For example, of is collapsed into the edge prep_of
      graph.nodes.filter(node => node.indices.max >= indices.min && node.indices.max <= indices.max)//.map(_.text).mkString(" ")
    }

    def makeString(nodes: List[DependencyNode]) = nodes.map(_.text).mkString(" ")
    
    val expandedArg1 = if (expandArgument) expand(arg1) else List(arg1)
    val expandedArg2 = if (expandArgument) expand(arg2) else List(arg2)
    if (Interval.span(expandedArg1.map(_.indices)) intersects Interval.span(expandedArg2.map(_.indices))) {
      logger.debug("invalid: arguments overlap: " + makeString(expandedArg1) + ", " + makeString(expandedArg2))
      None
    }
    else Some(new Extraction(makeString(expandedArg1), rel.text, Some(rel.text.split(" ").toSet -- PatternExtractor.LEMMA_BLACKLIST), makeString(expandedArg2)))
  }

  implicit def implicitBuildExtraction = this.buildExtraction(true)_
  implicit def implicitValidMatch = this.validMatch(false) _

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

  def loadTemplateExtractorsFromFile(patternFilePath: String): List[GeneralPatternExtractor] = {
    val patternSource = Source.fromFile(patternFilePath)
    val patterns: List[(Template, Pattern[DependencyNode], Int)] = try {
      // parse the file
      patternSource.getLines.map { line =>
        line.split("\t") match {
          // full information specified
          case Array(template, pat, count) => 
            (Template.deserialize(template), DependencyPattern.deserialize(pat), count.toInt)
          // assume a count of 1 if nothing is specified
          case Array(template, pat) => 
            logger.warn("warning: pattern has no count: " + pat); 
            (Template.deserialize(template), DependencyPattern.deserialize(pat), 1)
          case _ => throw new IllegalArgumentException("file can't have more than two columns")
        }
      }.toList
    } finally {
      patternSource.close
    }

    val maxCount = patterns.maxBy(_._3)._3
    (for ((template, pattern, count) <- patterns) yield {
      new TemplatePatternExtractor(template, pattern, count, maxCount)
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

      var showReverb: Boolean = false
      var duplicates: Boolean = false
      var expandArguments: Boolean = false
      var showAll: Boolean = false
      var verbose: Boolean = false
      var collapseVB: Boolean = false

      opt(Some("p"), "patterns", "<file>", "pattern file", { v: String => patternFilePath = Option(v) })
      opt(None, "lda", "<directory>", "lda directory", { v: String => ldaDirectoryPath = Option(v) })

      opt("d", "duplicates", "keep duplicate extractions", { duplicates = true })
      opt("x", "expand-arguments", "expand extraction arguments", { expandArguments = true })
      opt("r", "reverb", "show which extractions are reverb extractions", { showReverb = true })
      opt("collapse-vb", "collapse 'VB.*' to 'VB' in the graph", { collapseVB = true })

      opt("a", "all", "don't restrict extractions to are noun or adjective arguments", { showAll = true })
      opt("v", "verbose", "", { verbose = true })

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
      val extractors = ((parser.extractorType, distributions) match {
        case ("lda", Some(distributions)) => loadLdaExtractorsFromDistributions(distributions)
        case ("general", Some(distributions)) => loadGeneralExtractorsFromDistributions(distributions)
        case ("template", None) => 
          loadTemplateExtractorsFromFile(
            parser.patternFilePath.getOrElse(
              throw new IllegalArgumentException("pattern template file (--patterns) required for the template extractor.")))
        case ("specific", Some(distributions)) => loadSpecificExtractorsFromDistributions(distributions)
        case ("general", None) => 
          loadGeneralExtractorsFromFile(
            parser.patternFilePath.getOrElse(
              throw new IllegalArgumentException("pattern file (--patterns) required for the general extractor.")))
        case _ => throw new IllegalArgumentException("invalid parameters")
      }).toList
      
      /*
      logger.info("building reverse lookup")
      val reverseLookup = (for (extractor <- extractors; edge <- extractor.pattern.edgeMatchers.collect{case m: DependencyEdgeMatcher => m}) yield {
        (edge.label, extractor)
      }).foldLeft(Map[String, List[PatternExtractor]]().withDefaultValue(List())) { (acc, pair) => 
        acc + (pair._1 -> (pair._2 :: acc(pair._1))))
      }
      */

      implicit def implicitBuildExtraction = this.buildExtraction(parser.expandArguments)_
      implicit def implicitValidMatch = this.validMatch(!parser.showAll)_
      
      case class Result(conf: Double, extr: Extraction, text: String) extends Ordered[Result] {
        override def toString = text
        
        override def compare(that: Result) = this.conf.compare(that.conf)
      }
      
      val chunker = if (parser.showReverb) Some(new OpenNlpSentenceChunker) else None
      val reverb = if (parser.showReverb) Some(new ReVerbExtractor) else None
      
      def reverbExtract(sentence: String) = {
        import scala.collection.JavaConversions._
        val chunked = chunker.get.chunkSentence(sentence)
        val extractions = reverb.get.extract(chunked)
        extractions.map { extr =>
          val rs = new RelationString(extr.getRelation.getText, extr.getRelation.getTokens.map(MorphaStemmer.instance.lemmatize(_)).mkString(" "), extr.getRelation.getPosTags.mkString(" "))
          rs.correctNormalization()
          
          new Extraction(extr.getArgument1.getText, extr.getRelation.getText, Some(rs.getNormPred.split(" ").toSet -- PatternExtractor.LEMMA_BLACKLIST), extr.getArgument2.getText)
        }
      }
      
      logger.info("performing extractions")
      val sentenceSource = Source.fromFile(parser.sentenceFilePath)
      try {
        for (line <- sentenceSource.getLines) {
          val parts = line.split("\t")
          require(parts.length <= 2, "each line in sentence file must have no more than two columns: " + line)

          try {
            val dependencyString = parts.last
            val dependencies = Dependencies.deserialize(dependencyString)
            val text = if (parts.length > 1) Some(parts(0)) else None

            val rawDgraph = DependencyGraph(text, dependencies)
            val dgraph = if (parser.collapseVB) rawDgraph.simplifyPostags.simplifyVBPostags
            else rawDgraph.simplifyPostags

            if (text.isDefined) logger.debug("text: " + text.get)
            logger.debug("graph: " + Dependencies.serialize(dgraph.dependencies))

            if (parser.verbose) {
              if (text.isDefined) println("text: " + text.get)
              println("deps: " + dependencyString)
            }
            
            require(!parser.showReverb || text.isDefined, "original sentence text required to show reverb extractions")
            val reverbExtractions = if (!parser.showReverb) Nil else reverbExtract(text.get)
            if (parser.showReverb) {
              if (parser.verbose) println("reverb: " + reverbExtractions.mkString("[", "; ", "]"))
              logger.debug("reverb: " + reverbExtractions.mkString("[", "; ", "]"))
            }
            
            val results = for (
              extractor <- extractors;
              // todo: organize patterns by a reverse-lookup on edges
              // optimization: make sure the dependency graph contains all the edges
              if (extractor.pattern.edgeMatchers.forall(matcher => dependencies.exists(matcher.canMatch(_))));
              extr <- extractor.extract(dgraph) 
            ) yield {
              val conf = extractor.confidence(extr)
              val reverbMatches = reverbExtractions.find(_.softMatch(extr))
              logger.debug("reverb match: " + reverbMatches.toString)
              val extra = reverbMatches.map("\treverb:" + _.toString)

              val resultText =
                if (parser.verbose) "extraction: "+("%1.6f" format conf)+" " + extr.toString + " with (" + extractor.pattern.toString + ")" + reverbMatches.map(" compared to " + _).getOrElse("")
                else ("%1.6f" format conf) + "\t" + extr + "\t" + extractor.pattern + "\t" + ("" /: text)((_, s) => s + "\t") + dependencyString + extra.getOrElse("")
              Result(conf, extr, resultText)
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

            if (parser.verbose) println()
          }
          catch {
            case e: DependencySerializationException => logger.warn(e.getMessage)
          }
        }
      } finally {
        sentenceSource.close
      }
    }
  }
}
