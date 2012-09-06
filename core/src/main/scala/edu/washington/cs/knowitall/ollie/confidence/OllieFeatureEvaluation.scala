package edu.washington.cs.knowitall.ollie.confidence

import edu.washington.cs.knowitall.openparse.OpenParse.Configuration
import java.io.PrintWriter
import java.io.File
import java.net.URL
import edu.washington.cs.knowitall.openparse.OpenParse
import edu.washington.cs.knowitall.common.Resource.using
import scopt.OptionParser
import scala.io.Source
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.ollie.Ollie
import edu.washington.cs.knowitall.openparse.eval.Score
import edu.washington.cs.knowitall.common.Analysis
import edu.washington.cs.knowitall.ollie.OllieExtractionInstance

object OllieFeatureEvaluation {
    /** Settings for OpenParse. */
  abstract class Settings {
    /** url to find the model */
    def modelUrl: URL

    /** url to confidence model */
    def confidenceModelUrl: URL

    /** file with annotations */
    def goldFile: File

    /** file to output; None means stdout */
    def outputFile: Option[File]

    /** source file of dependencies */
    def inputFile: File

    /** threshold for reported extractions and attempted patterns */
    def confidenceThreshold: Double

    /** Create an OpenParse configuration from the settings. */
    def configuration: Configuration = new Configuration(
      confidenceThreshold = this.confidenceThreshold)
  }

  def main(args: Array[String]) = {
    var settings = new Settings {
      var modelUrl: URL = OpenParse.defaultModelUrl
      var confidenceModelUrl: URL = OllieIndependentConfFunction.defaultModelUrl
      var outputFile: Option[File] = None
      var goldFile: File = _
      var inputFile: File = _
      var confidenceThreshold: Double = 0.0
    }

    val parser = new OptionParser("feature-eval") {
      opt(Some("m"), "model", "<file>", "model file", { path: String =>
        val file = new File(path)
        require(file.exists, "file does not exist: " + path)
        settings.modelUrl = file.toURI.toURL
      })
      opt(Some("c"), "confidence model", "<file>", "confidence model file", { path: String =>
        val file = new File(path)
        require(file.exists, "file does not exist: " + path)
        settings.confidenceModelUrl = file.toURI.toURL
      })
      doubleOpt(Some("t"), "threshold", "<threshold>", "confident threshold for shown extractions", { t: Double => settings.confidenceThreshold = t })
      opt("o", "output", "output file (otherwise stdout)", { path =>
        val file = new File(path)
        settings.outputFile = Some(file)
      })

      arg("input", "input dependencies file", { path: String =>
        val file = new File(path)
        require(file.exists, "input file does not exist: " + path)
        settings.inputFile = file
      })

      arg("gold", "gold file", { path: String =>
        val file = new File(path)
        require(file.exists, "gold file does not exist: " + path)
        settings.goldFile = file
      })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }

  def run(settings: Settings) = {
    val extractor = new Ollie(OpenParse.fromModelUrl(settings.modelUrl, settings.configuration))
    val gold = Score.loadGoldSet(settings.goldFile)
    val confFunc = OllieIndependentConfFunction.fromUrl(OllieFeatureSet, settings.confidenceModelUrl)

    case class Scored(conf: Double, inst: OllieExtractionInstance, score: Boolean)

    val extrs = using (Source.fromFile(settings.inputFile)) { source =>
      for (
        line <- source.getLines.toList;
        val graph = DependencyGraph.deserialize(line);
        inst <- extractor.extract(graph);
        score <- gold.get(inst.extr.toString);
        val conf = confFunc(inst)
      ) yield Scored(conf, inst, score)
    }

    val sorted = extrs.sortBy(-_.conf).toList

    val pyed = (sorted.head, 0, 1.0) +: Analysis.precisionYieldMeta(sorted zip sorted.map(_.score))

    val featureNames = confFunc.featureSet.featureNames.filter(confFunc.featureWeights.get(_).isDefined).toList.sorted
    using {
      settings.outputFile match {
        case Some(f) => new PrintWriter(f, "UTF8")
        case None => new PrintWriter(System.out)
      }
    } { writer =>
      writer.println((Iterable("score", "conf", "op-conf", "yield", "precision",
        "extr", "enabler", "attrib", "sentence", "dependencies") ++
        featureNames).mkString("\t"))
      writer.println("\t" * 10 + featureNames.map(confFunc.featureWeights(_).toString).mkString("\t"))
    (pyed) foreach { case (scored, y, p) =>
      val features =
        for (
          featureName <- featureNames;
          val featureValue = confFunc.featureSet(featureName)(scored.inst)
        ) yield featureValue

      writer.println((Iterable(if (scored.score) 1 else 0,
          scored.conf,
          scored.inst.extr.openparseConfidence,
          y,
          p,
          scored.inst.extr.toString,
          scored.inst.extr.enabler.isDefined.toString.toLowerCase,
          scored.inst.extr.attribution.isDefined.toString.toLowerCase,
          scored.inst.sent.text,
          scored.inst.sent.serialize) ++ features).mkString("\t"))
      }
    }
  }
}
