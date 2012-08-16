package edu.washington.cs.knowitall.openparse

import java.io.{PrintWriter, File}
import java.net.URL

import scala.collection.Set
import scala.io.Source

import org.slf4j.LoggerFactory

import edu.washington.cs.knowitall.collection.immutable.graph.pattern.Match
import edu.washington.cs.knowitall.collection.immutable.graph.Graph
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.common.Timing
import edu.washington.cs.knowitall.tool.parse.MaltParser
import edu.washington.cs.knowitall.openparse.OpenParse.validMatch
import edu.washington.cs.knowitall.openparse.extract.{TemplateExtractor, PatternExtractorType, PatternExtractor, GeneralExtractor, Extraction, DetailedExtraction}
import edu.washington.cs.knowitall.tool.parse.graph.{DependencyNode, DependencyGraph}

import scopt.OptionParser

object OpenParseCli {
  val logger = LoggerFactory.getLogger(this.getClass)

  abstract class Settings {
    def modelUrl: URL
    def outputFile: Option[File]
    def sentenceFile: File

    def confidenceThreshold: Double
    def expandArguments: Boolean
    def verbose: Boolean

    def parallel: Boolean
    def invincible: Boolean
  }

  def main(args: Array[String]) {
    object settings extends Settings {
      var modelUrl: URL = OpenParse.defaultModelUrl
      var outputFile: Option[File] = None
      var sentenceFile: File = null

      var confidenceThreshold = 0.0;
      var expandArguments: Boolean = true
      var verbose: Boolean = false

      var parallel: Boolean = false
      var invincible: Boolean = false
    }

    val parser = new OptionParser("openparse-cli") {
      arg("sentences", "sentence file", { path: String => 
        val file = new File(path)
        require(file.exists, "file does not exist: " + path)
        settings.sentenceFile = file
      })
      opt(Some("m"), "model", "<file>", "model file", { path: String => 
        val file = new File(path)
        require(file.exists, "file does not exist: " + path)
        settings.modelUrl = file.toURI.toURL 
      })
      doubleOpt(Some("t"), "threshold", "<threshold>", "confident threshold for shown extractions", { t: Double => settings.confidenceThreshold = t })
      opt("o", "output", "output file (otherwise stdout)", { path => settings.outputFile = Some(new File(path)) })

      opt("x", "expand-arguments", "expand extraction arguments", { settings.expandArguments = true })
      opt("v", "verbose", "", { settings.verbose = true })

      opt("p", "parallel", "", { settings.parallel = true })
      opt("invincible", "", { settings.invincible = true })
    }

    if (parser.parse(args)) {
      logger.info("args: " + args.mkString(" "))
      run(settings)
    }
  }

  def run(settings: Settings) {
    val parser = new MaltParser
    def parse(line: String): Option[DependencyGraph] = {
      Some(parser.dependencyGraph(line))
    }

    val other = new OpenParse.Settings {
      var modelUrl = settings.modelUrl
      var outputFile = settings.outputFile
      var sentenceFile = settings.sentenceFile 
      var confidenceThreshold = settings.confidenceThreshold 
      val duplicates = false
      var expandArguments = settings.expandArguments 
      val showAll = false
      var verbose = settings.verbose 
      val collapseVB = false
      var parallel = settings.parallel 
      var invincible = settings.invincible 
    }

    OpenParse.run(other, parse)
  }
}
