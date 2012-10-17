package edu.washington.cs.knowitall.ollie.confidence

import java.io.File
import java.io.PrintWriter
import java.net.URL

import scala.io.Source

import edu.washington.cs.knowitall.common.Analysis
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.ollie.ScoredOllieExtractionInstance
import scopt.OptionParser

object OllieFeatureEvaluation {
    /** Settings for OpenParse. */
  abstract class Settings {
    /** source file of scored extractions */
    def inputFile: File

    /** file to output; None means stdout */
    def outputFile: Option[File]

    /** confidence model url */
    def confidenceModelUrl: URL
  }

  def main(args: Array[String]) = {
    var settings = new Settings {
      var inputFile: File = _
      var outputFile: Option[File] = None
      var confidenceModelUrl: URL = OllieIndependentConfFunction.defaultModelUrl
    }

    val parser = new OptionParser("feature-eval") {
      opt(Some("c"), "confidence model", "<file>", "confidence model file", { path: String =>
        val file = new File(path)
        require(file.exists, "file does not exist: " + path)
        settings.confidenceModelUrl = file.toURI.toURL
      })

      opt("o", "output", "output file (otherwise stdout)", { path =>
        val file = new File(path)
        settings.outputFile = Some(file)
      })

      arg("input", "input dependencies file", { path: String =>
        val file = new File(path)
        require(file.exists, "input file does not exist: " + path)
        settings.inputFile = file
      })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }

  def run(settings: Settings) = {
    val confFunc = OllieIndependentConfFunction.fromUrl(OllieFeatureSet, settings.confidenceModelUrl)

    val extrs = using (Source.fromFile(settings.inputFile)) { source =>
      for (
        line <- source.getLines.toList;
        val scored = ScoredOllieExtractionInstance.tabDeserialize(line);
        val conf = confFunc(scored.inst)
      ) yield (conf, scored)
    }

    val sorted = extrs.sortBy(-_._1).toList

    val pyed = (sorted.head, 0, 1.0) +: Analysis.precisionYieldMeta(sorted zip sorted.map(_._2.score))

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
    (pyed) foreach { case ((conf, scored), y, p) =>
      val features =
        for (
          featureName <- featureNames;
          val featureValue = confFunc.featureSet(featureName)(scored.inst)
        ) yield featureValue

      writer.println((Iterable(if (scored.score) 1 else 0,
          conf,
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
