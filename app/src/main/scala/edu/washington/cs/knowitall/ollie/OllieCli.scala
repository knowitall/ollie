package edu.washington.cs.knowitall.ollie;

import java.io.File
import java.io.PrintWriter

import scala.io.Source

import OllieCli.Settings
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.ollie.confidence.OllieIndependentConfFunction
import edu.washington.cs.knowitall.openparse.OpenParse
import edu.washington.cs.knowitall.tool.parse.StanfordParser
import scopt.OptionParser

class OllieCli {
  def main(args: Array[String]): Unit = {
    object settings extends Settings {
      var inputFile: Option[File] = None
      var outputFile: Option[File] = None
      var confidenceThreshold: Double = 0.0
    }

    val argumentParser = new OptionParser("ollie") {
      argOpt("<input-file>", "pattern file", { path: String =>
        settings.inputFile = Some(new File(path))
      })

      opt(Some("o"), "output", "<output-file>", "output file (otherwise stdout)", { path: String =>
        settings.outputFile = Some(new File(path))
      })

      doubleOpt(Some("t"), "threshold", "<threshold>", "confidence threshold for OpenParse extractor component", { t: Double =>
        settings.confidenceThreshold = t
      })
    }

    if (argumentParser.parse(args)) {
      val parser = new StanfordParser()
      val ollieExtractor = new Ollie(OpenParse.fromModelUrl(OpenParse.defaultModelUrl))

      val confFunction = OllieIndependentConfFunction.loadDefaultClassifier

      using(settings.inputFile match {
        case Some(input) => Source.fromFile(input, "UTF-8")
        case None => Source.stdin
      }) { source =>

        using(settings.outputFile match {
          case Some(output) => new PrintWriter(output, "UTF-8")
          case None => new PrintWriter(System.out)
        }) { writer =>

          for (line <- source.getLines) {
            println(line)
            ollieExtractor.extract(parser.dependencyGraph(line)).foreach { e =>
              val conf = confFunction.getConf(e)
              println(conf + ": " + e.extr)
            }

            println()
          }
        }
      }
    }
  }
}

object OllieCli {
  abstract class Settings {
    def inputFile: Option[File]
    def outputFile: Option[File]
    def confidenceThreshold: Double
  }
}