
package edu.washington.cs.knowitall.ollie.confidence.train

import java.io.File

import scala.io.Source

import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.ollie.ScoredOllieExtractionInstance
import edu.washington.cs.knowitall.ollie.confidence.OllieFeatureSet
import edu.washington.cs.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import scopt.mutable.OptionParser

object TrainOllieConfidence {
  def main(args: Array[String]) {
    object settings extends Settings {
      var inputFile: File = _
      var outputFile: Option[File] = None
    }

    val parser = new OptionParser("scoreextr") {
      arg("labelled", "labelled extractions", { path: String => settings.inputFile = new File(path) })
      argOpt("output", "output file", { path: String => settings.outputFile = Some(new File(path)) })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }

   abstract class Settings {
     def inputFile: File
     def outputFile: Option[File]
   }

  def run(settings: Settings) = {
    val trainer = new BreezeLogisticRegressionTrainer(OllieFeatureSet)

    val data =
      using (Source.fromFile(settings.inputFile)) { source =>
        (source.getLines map (ScoredOllieExtractionInstance.tabDeserialize)).toList
      }

    val classifier = trainer.train(data)
    settings.outputFile match {
      case Some(file) => classifier.saveFile(file)
      case None =>
        classifier.save(System.out)
    }
  }
}