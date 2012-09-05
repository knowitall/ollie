package edu.washington.cs.knowitall.ollie.confidence.train

import java.io.File
import java.io.PrintWriter

import scala.io.Source

import breeze.classify.LogisticClassifier
import breeze.data.Example
import breeze.linalg.DenseVector
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.ollie.OllieExtractionInstance
import edu.washington.cs.knowitall.ollie.ScoredOllieExtractionInstance
import edu.washington.cs.knowitall.ollie.confidence.FeatureSet
import edu.washington.cs.knowitall.ollie.confidence.OllieFeatureSet
import edu.washington.cs.knowitall.ollie.confidence.OllieIndependentConfFunction
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
    val trainer = new TrainOllieConfidence(OllieFeatureSet)

    val data =
      using (Source.fromFile(settings.inputFile)) { source =>
        (source.getLines map (ScoredOllieExtractionInstance.tabDeserialize)).toList
      }

    val classifier = trainer.train(data)
    settings.outputFile match {
      case Some(file) => classifier.saveFile(file)
      case None =>
        using (new PrintWriter(System.out)) { writer =>
          classifier.save(writer)
        }
    }
  }
}

class TrainOllieConfidence(val features: FeatureSet[OllieExtractionInstance]) {
  def trainBreezeClassifier(instances: Iterable[ScoredOllieExtractionInstance]) = {
    val examples = instances.zipWithIndex map { case (scored, i) =>
      val vector = DenseVector(features.vectorize(scored.inst).toArray)
      Example[Boolean, DenseVector[Double]](scored.score, vector, id=i.toString)
    }

    new LogisticClassifier.Trainer[Boolean,DenseVector[Double]].train(examples)
  }

  def train(instances: Iterable[ScoredOllieExtractionInstance]) = {
    val classifier = trainBreezeClassifier(instances)

    val weights = (features.featureNames.iterator zip classifier.featureWeights.indexed(true).iterator.map(_._2)).toMap
    new OllieIndependentConfFunction(OllieFeatureSet, weights, 0.0)
  }
}