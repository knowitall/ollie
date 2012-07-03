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
import edu.washington.cs.knowitall.tool.sentence.OpenNlpSentencer
import edu.washington.cs.knowitall.tool.sentence.Sentencer
import edu.washington.cs.knowitall.common.Timing

/** An entry point to use Ollie on the command line.
  */
object OllieCli {
  /** A definition of command line arguments.
    */
  abstract class Settings {
    def inputFile: Option[File]
    def outputFile: Option[File]
    def confidenceThreshold: Double

    def splitInput: Boolean
    def tabbed: Boolean
    def parallel: Boolean
    def invincible: Boolean
  }

  /** Size to group for parallelism. */
  private val CHUNK_SIZE = 10000

  def main(args: Array[String]): Unit = {
    object settings extends Settings {
      var inputFile: Option[File] = None
      var outputFile: Option[File] = None
      var confidenceThreshold: Double = 0.0

      var splitInput: Boolean = false
      var tabbed: Boolean = false
      var parallel: Boolean = false
      var invincible: Boolean = false
    }

    // define the argument parser
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

      opt("t", "tabbed", "output in TSV format", { settings.tabbed = true })
      opt("s", "split", "split input text", { settings.splitInput = true })
      opt("p", "parallel", "execute in parallel", { settings.parallel = true })
      opt("invincible", "ignore errors", { settings.invincible = true })
    }

    if (argumentParser.parse(args)) {
      run(settings)
    }
  }

  def run(settings: Settings) = {
    System.err.println("Loading models...")
    val parser = new StanfordParser()

    val ollieExtractor = new Ollie(OpenParse.fromModelUrl(OpenParse.defaultModelUrl))
    val confFunction = OllieIndependentConfFunction.loadDefaultClassifier

    val sentencer =
      if (settings.splitInput) Some(new OpenNlpSentencer)
      else None

    System.err.println("\nRunning extractor...")
    using(settings.inputFile match {
      case Some(input) => Source.fromFile(input, "UTF-8")
      case None => Source.stdin
    }) { source =>

      using(settings.outputFile match {
        case Some(output) => new PrintWriter(output, "UTF-8")
        case None => new PrintWriter(System.out)
      }) { writer =>

        if (settings.tabbed) println(Iterable("confidence", "arg1", "rel", "arg2", "enabler", "attribution", "dependencies", "text").mkString("\t"))
        val ns = Timing.time {
          val lines = parseLines(source.getLines, sentencer)
          try {
            // group the lines so we can parallelize
            for (group <- lines.grouped(CHUNK_SIZE)) {
              // potentially transform to a parallel collection
              val sentences = if (settings.parallel) group.par else group
              for (sentence <- sentences) {
                if (!settings.tabbed) println("'" + sentence + "'")

                // parse the sentence
                val graph = parser.dependencyGraph(sentence)

                // extract sentence and compute confidence
                val extrs = ollieExtractor.extract(graph).map(extr => (confFunction.getConf(extr), extr))
                
                extrs.toSeq.sortBy(-_._1).foreach { case (conf, e) =>
                  if (settings.tabbed) {
                    println(Iterable(conf, e.extr.arg1.text, e.extr.rel.text, e.extr.arg2.text, e.extr.enabler, e.extr.attribution, e.sent.serialize, e.sent.text).mkString("\t"))
                  } else {
                    println(conf + ": " + e.extr)
                  }
                }

                if (!settings.tabbed) println()
              }
            }
          } catch {
            case e: Exception if settings.invincible => e.printStackTrace
          }
        }
        
        System.err.println("completed in " + Timing.Seconds.format(ns) + " seconds")
      }
    }
  }

  def parseLines(lines: Iterator[String], sentencer: Option[Sentencer]) = {
    sentencer match {
      case None => lines
      case Some(sentencer) =>
        new Iterator[String]() {
          var sentences: Iterator[String] = Iterator.empty

          def hasNext = sentences.hasNext || lines.hasNext
          def next = {
            if (sentences.hasNext) {
              sentences.next()
            } else {
              lines.dropWhile(_.trim.isEmpty) // skip empty lines
              val text = lines.takeWhile(!_.trim.isEmpty).mkString(" ")
              sentences = sentencer.sentences(text).iterator
              sentences.next()
            }
          }
        }.filter(!_.trim.isEmpty)
    }
  }
}