package edu.washington.cs.knowitall
package pattern

import java.io.File
import java.io.PrintWriter

import scala.collection.Set
import scala.io.Source

import org.slf4j.LoggerFactory

import OpenParse.validMatch
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.common.Timing
import edu.washington.cs.knowitall.pattern.extract.PatternExtractorType
import edu.washington.cs.knowitall.tool.parse.graph.Graph
import edu.washington.cs.knowitall.tool.parse.pattern.Match
import edu.washington.cs.knowitall.tool.postag.PosTagger
import extract.DetailedExtraction
import extract.Extraction
import extract.GeneralExtractor
import extract.PatternExtractor
import extract.TemplateExtractor
import scopt.OptionParser
import tool.parse.graph.DependencyGraph
import tool.parse.graph.DependencyNode

object OpenParse {
  val LEMMA_BLACKLIST = PosTagger.simplePrepositions + "like" + "be"
  val VALID_ARG_POSTAG = Set("NN", "NNS", "NNP", "NNPS", "JJ", "JJS", "CD", "PRP")
  val logger = LoggerFactory.getLogger(this.getClass)

  val CHUNK_SIZE = 10000

  implicit def implicitBuildExtraction = Extraction.fromMatch(true) _
  implicit def implicitValidMatch = validMatch(true) _

  def validMatch(restrictArguments: Boolean)(graph: Graph[DependencyNode])(m: Match[DependencyNode]) = {
    !restrictArguments || VALID_ARG_POSTAG.contains(m.nodeGroups("arg1").node.postag) && VALID_ARG_POSTAG.contains(m.nodeGroups("arg2").node.postag)
  }

  def loadExtractors(extractorType: PatternExtractorType,
    patternFile: Option[File]): List[PatternExtractor] =
    {
      logger.info("reading patterns")

      // sort by inverse count so frequent patterns appear first 
      (extractorType match {
        case TemplateExtractor =>
          TemplateExtractor.fromFile(
            patternFile.getOrElse(
              throw new IllegalArgumentException("pattern template file (--patterns) required for the template extractor.")))
        case GeneralExtractor =>
          GeneralExtractor.fromFile(
            patternFile.getOrElse(
              throw new IllegalArgumentException("pattern file (--patterns) required for the general extractor.")))
        case _ => throw new IllegalArgumentException("invalid parameters")
      }).toList
    }

  private case class Result(conf: Double, extr: DetailedExtraction, text: String) extends Ordered[Result] {
    override def toString = text

    override def compare(that: Result) = {
      val conf = this.conf.compare(that.conf)

      if (conf == 0) (this.extr.toString + text).compareTo(that.extr.toString + text)
      else conf
    }
  }

  abstract class Settings {
    def patternFile: Option[File]
    def outputFile: Option[File]

    def sentenceFilePath: String
    def extractorType: PatternExtractorType

    def confidenceThreshold: Double

    def duplicates: Boolean
    def expandArguments: Boolean
    def showAll: Boolean
    def verbose: Boolean
    def collapseVB: Boolean

    def parallel: Boolean
    def invincible: Boolean

    def configuration = new Configuration(
      confidenceThreshold = this.confidenceThreshold,
      expandExtraction = this.expandArguments,
      simplifyVBPostags = this.collapseVB,
      keepDuplicates = this.duplicates)
  }

  def main(args: Array[String]) {
    object settings extends Settings {
      var patternFile: Option[File] = None
      var outputFile: Option[File] = None

      var sentenceFilePath: String = null
      var extractorType: PatternExtractorType = null

      var confidenceThreshold = 0.0;

      var duplicates: Boolean = false
      var expandArguments: Boolean = false
      var showAll: Boolean = false
      var verbose: Boolean = false
      var collapseVB: Boolean = false

      var parallel: Boolean = false
      var invincible: Boolean = false
    }

    val parser = new OptionParser("applypat") {
      opt(Some("p"), "patterns", "<file>", "pattern file", { v: String => settings.patternFile = Option(new File(v)) })
      doubleOpt(Some("t"), "threshold", "<threshold>", "confident threshold for shown extractions", { t: Double => settings.confidenceThreshold = t })
      opt("o", "output", "output file (otherwise stdout)", { path => settings.outputFile = Some(new File(path)) })

      opt("d", "duplicates", "keep duplicate extractions", { settings.duplicates = true })
      opt("x", "expand-arguments", "expand extraction arguments", { settings.expandArguments = true })
      opt("collapse-vb", "collapse 'VB.*' to 'VB' in the graph", { settings.collapseVB = true })

      opt("a", "all", "don't restrict extractions to are noun or adjective arguments", { settings.showAll = true })
      opt("v", "verbose", "", { settings.verbose = true })
      opt("p", "parallel", "", { settings.parallel = true })
      opt("invincible", "", { settings.invincible = true })

      arg("type", "type of extractor", { v: String => settings.extractorType = PatternExtractorType(v) })
      arg("sentences", "sentence file", { v: String => settings.sentenceFilePath = v })
    }

    if (parser.parse(args)) {
      logger.info("args: " + args.mkString(" "))
      run(settings)
    }
  }

  class Configuration(
    val simplifyVBPostags: Boolean = false,
    val simplifyPostags: Boolean = true,
    val confidenceThreshold: Double = 0.0,
    val expandExtraction: Boolean = true,
    val restrictArguments: Boolean = true,
    val keepDuplicates: Boolean = false)

  def run(settings: Settings) {
    // load the individual extractors
    val extractors = loadExtractors(settings.extractorType, settings.patternFile)

    // create a standalone extractor
    val configuration = settings.configuration
    val extractor = new OpenParse(extractors, configuration)

    logger.info("performing extractions")
    var chunkCount = 0
    val extractionCount = new java.util.concurrent.atomic.AtomicInteger
    val startTime = System.nanoTime();
    using(settings.outputFile.map(new PrintWriter(_)).getOrElse(new PrintWriter(System.out))) { writer =>
      using(Source.fromFile(settings.sentenceFilePath, "UTF-8")) { sentenceSource =>
        for (group <- sentenceSource.getLines.grouped(CHUNK_SIZE)) {
          chunkCount = chunkCount + 1
          if (settings.outputFile.isDefined) {
            // provide some nice information about where we are
            println("summary: " + extractionCount + " extractions, " + chunkCount * CHUNK_SIZE + " sentences, " + Timing.Seconds.format(System.nanoTime() - startTime) + " seconds");
          }
          val lines = if (settings.parallel) group.par else group

          for (line <- lines) {
            try {
              val parts = line.split("\t")
              require(parts.length <= 2, "each line in sentence file must have no more than two columns: " + line)

              try {
                val pickled = parts.last
                val dgraph = DependencyGraph.deserialize(pickled)
                val text = if (parts.length > 1) parts(0) else dgraph.text
                logger.debug("text: " + text)
                logger.debug("graph: " + pickled)

                if (settings.verbose) {
                  writer.println("text: " + text)
                  writer.println("deps: " + pickled)
                }

                val extractions = extractor.extract(dgraph)
                extractionCount.addAndGet(extractions.size)
                val results = for ((conf, extr) <- extractions) yield {
                  val resultText =
                    if (settings.verbose) "extraction: " + ("%1.6f" format conf) + " " + extr.toString + " with (" + extr.extractor.pattern.toString + ")"
                    else ("%1.6f" format conf) + "\t" + extr + "\t" + extr.extractor.pattern + "\t" + text + "\t" + pickled
                  Result(conf, extr, resultText)
                }

                writer.synchronized {
                  for (result <- results) {
                    writer.println(result)
                  }
                }

                if (settings.verbose) writer.println()
              } catch {
                case e: DependencyGraph.SerializationException => logger.error("could not deserialize graph.", e)
              }
            } catch {
              case e if settings.invincible =>
              case e => throw e
            }
          }
        }
      }
    }
  }
}

class OpenParse(extractors: Seq[PatternExtractor], configuration: OpenParse.Configuration) {
  import OpenParse._

  def this(extractors: Seq[PatternExtractor]) =
    this(extractors, new OpenParse.Configuration())

  def simplifyGraph(dgraph: DependencyGraph) = {
    var graph = dgraph

    if (configuration.simplifyPostags) {
      graph = dgraph.simplifyPostags
    }

    if (configuration.simplifyVBPostags) {
      graph = dgraph.simplifyVBPostags
    }

    graph
  }

  def extract(dg: DependencyGraph) = {
    val dgraph = simplifyGraph(dg)

    /**
     * Check if the PatternExtractor gives a deterministic confidence.
     * If so, make sure it is above the threshold.
     */
    def confidenceOverThreshold(extractor: PatternExtractor, threshold: Double) = {
      extractor.confidence match {
        // there is an independent confidence so do the check
        case Some(conf) => conf >= configuration.confidenceThreshold
        // there is no independent confidence, so we need to continue
        // and compare the dependent confidence
        case None => true
      }
    }

    /**
     * Quick checks to see if an extraction is possible.  This
     * is an optimization, so the checks should be considerably
     * faster than running the extractors.
     */
    def possibleExtraction(extractor: PatternExtractor, dgraph: DependencyGraph) = {
      extractor.pattern.edgeMatchers.forall { matcher =>
        dgraph.dependencies.exists(matcher.canMatch(_))
      }
    }

    // implicit methods on individual extractors
    val build = Extraction.fromMatch(configuration.expandExtraction) _
    val valid = validMatch(configuration.restrictArguments) _

    val extrs = for {
      extractor <- extractors;
      // todo: organize patterns by a reverse-lookup on edges

      // optimizations
      if (confidenceOverThreshold(extractor, configuration.confidenceThreshold));
      if (possibleExtraction(extractor, dgraph));

      // extraction
      extr <- extractor.extract(dgraph)(build, valid)
      val conf = extractor.confidence(extr);
      if conf >= configuration.confidenceThreshold
    } yield {
      (conf, extr)
    }

    val reduced = if (configuration.keepDuplicates) {
      extrs
    } else {
      // toSet to remove exact duplicates
      val set = extrs.toSet

      val reduced = set filterNot {
        case (thisConf, thisExtr) =>
          set exists {
            case (thatConf, thatExtr) =>
              // the relations are identical
              thisExtr.rel == thatExtr.rel &&
                // this confidence is lower than that
                thisConf < thatConf &&
                // one of the other argument is a substring of the other
                (thatExtr.arg1Text.contains(thisExtr.arg1Text) || thatExtr.arg2Text.contains(thisExtr.arg2Text))
          }
      }

      reduced.toSeq
    }

    reduced.sortBy { case (conf, extr) => (-conf, extr.toString) }
  }
}
