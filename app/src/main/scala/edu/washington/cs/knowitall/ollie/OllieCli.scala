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
import edu.washington.cs.knowitall.tool.sentence.Sentencer
import edu.washington.cs.knowitall.common.Timing
import edu.washington.cs.knowitall.tool.sentence.OpenNlpSentencer
import java.text.DecimalFormat

/** An entry point to use Ollie on the command line.
  */
object OllieCli {
  /** A definition of command line arguments.
    */
  abstract class Settings {
    def inputFile: Option[File]
    def outputFile: Option[File]
    def confidenceThreshold: Double
    def openparseConfidenceThreshold: Double

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
      var openparseConfidenceThreshold: Double = 0.005

      var splitInput: Boolean = false
      var tabbed: Boolean = false
      var parallel: Boolean = false
      var invincible: Boolean = false
      
      var showUsage: Boolean = false
    }

    // define the argument parser
    val argumentParser = new OptionParser("ollie") {
      argOpt("<input-file>", "pattern file", { path: String =>
        settings.inputFile = Some(new File(path))
      })

      opt(Some("o"), "output", "<output-file>", "output file (otherwise stdout)", { path: String =>
        settings.outputFile = Some(new File(path))
      })

      opt("h", "help", "usage information", { settings.showUsage = true })

      doubleOpt(Some("t"), "threshold", "<threshold>", "confidence threshold for Ollie extractor", { t: Double =>
        settings.confidenceThreshold = t
      })
      
      doubleOpt(None, "openparse-threshold", "<threshold>", "confidence threshold for OpenParse component", { t: Double =>
        settings.openparseConfidenceThreshold = t
      })

      opt("p", "parallel", "execute in parallel", { settings.parallel = true })
      opt("s", "split", "split text into sentences", { settings.splitInput = true })
      opt("tabbed", "output in TSV format", { settings.tabbed = true })
      opt("ignore-errors", "ignore errors", { settings.invincible = true })
      opt("usage", "this usage message", { settings.showUsage = true })
    }

    if (argumentParser.parse(args)) {
      if (settings.showUsage) {
        println()
        println("Ollie takes sentences as input, one per line.")
        println("The response is \"confidence: extraction\", one extraction per line.")
        println(argumentParser.usage)
      }
      else {
        run(settings)
      }
    }
  }

  def run(settings: Settings) = {
    System.err.println("Loading models...")
    val parser = new StanfordParser()

    val openparse = OpenParse.withDefaultModel(
        new OpenParse.Configuration(
            confidenceThreshold = settings.openparseConfidenceThreshold))
    val ollieExtractor = new Ollie(openparse)
    val confFunction = OllieIndependentConfFunction.loadDefaultClassifier

    val sentencer = if (settings.splitInput) Some(new OpenNlpSentencer()) else None
    
    val confFormatter = new DecimalFormat("#.###")
    
    System.err.println("\nRunning extractor on " + (settings.inputFile match { case None => "standard input" case Some(f) => f.getName }) + "...")
    using(settings.inputFile match {
      case Some(input) => Source.fromFile(input, "UTF-8")
      case None => Source.stdin
    }) { source =>

      using(settings.outputFile match {
        case Some(output) => new PrintWriter(output, "UTF-8")
        case None => new PrintWriter(System.out)
      }) { writer =>
        if (settings.tabbed) writer.println(Iterable("confidence", "arg1", "rel", "arg2", "enabler", "attribution", "dependencies", "text").mkString("\t"))
        val ns = Timing.time {
          // print prompt if standard input
          if (!settings.outputFile.isDefined) {
            System.out.print("> ")
            System.out.flush()
          }

          val lines = parseLines(source.getLines, sentencer) filter (!_.isEmpty)
          try {
            // group the lines so we can parallelize
            val grouped = if (settings.parallel) lines.grouped(CHUNK_SIZE) else lines.map(Seq(_))
            for (group <- grouped) {

              // potentially transform to a parallel collection
              val sentences = if (settings.parallel) group.par else group
              for (sentence <- sentences) {
                if (!settings.tabbed) {
                  writer.println(sentence)
                  writer.flush()
                }

                // parse the sentence
                val graph = parser.dependencyGraph(sentence)

                // extract sentence and compute confidence
                val extrs = ollieExtractor.extract(graph).map(extr => (confFunction.getConf(extr), extr))

                extrs match {
                  case Nil if !settings.tabbed => writer.println("No extractions found.")
                  case Nil =>
                  case extrs => (extrs filter (_._1 >= settings.confidenceThreshold)).toSeq.sortBy(-_._1).foreach {
                    case (conf, e) =>
                      if (settings.tabbed) {
                        writer.println(Iterable(confFormatter.format(conf), 
                          e.extr.arg1.text, 
                          e.extr.rel.text, 
                          e.extr.arg2.text, 
                          e.extr.enabler.map(_.text), 
                          e.extr.attribution.map(_.text), 
                          e.sent.text, 
                          e.sent.serialize).mkString("\t"))
                      } else {
                        writer.println(confFormatter.format(conf) + ": " + e.extr)
                      }

                      writer.flush()
                  }
                }

                if (!settings.tabbed) { 
                  writer.println()
                  writer.flush()
                }
              }

              // print prompt if standard input
              if (!settings.outputFile.isDefined) {
                System.out.print("> ")
                System.out.flush()
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
      case Some(sentencer) => new SentenceIterator(sentencer, lines.buffered)
    }
  }
}
