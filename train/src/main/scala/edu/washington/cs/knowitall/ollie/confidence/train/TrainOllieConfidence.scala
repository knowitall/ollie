package edu.washington.cs.knowitall.ollie.confidence.train

import scala.io.Source
import breeze.classify._
import breeze.data.Example
import breeze.linalg.DenseVector
import scopt.mutable.OptionParser
import edu.washington.cs.knowitall.ollie.OllieExtractionInstance
import edu.washington.cs.knowitall.ollie.ScoredOllieExtractionInstance
import edu.washington.cs.knowitall.ollie.confidence.FeatureSet
import edu.washington.cs.knowitall.ollie.confidence.OllieFeatureSet
import edu.washington.cs.knowitall.common.Resource.using
import java.io.File

object TrainOllieConfidence {
  def main(args: Array[String]) {
    object settings extends Settings {
      var inputFile: File = _
    }

    val parser = new OptionParser("scoreextr") {
      arg("labelled", "labelled extractions", { path: String => settings.inputFile = new File(path) })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }

   abstract class Settings {
     def inputFile: File
   }


  def run(settings: Settings) = {
    val trainer = new TrainOllieConfidence(OllieFeatureSet)

    val data =
      using (Source.fromFile(settings.inputFile)) { source =>
        (source.getLines map (ScoredOllieExtractionInstance.tabDeserialize)).toList
      }

    trainer.train(data)
  }
}

class TrainOllieConfidence(val features: FeatureSet[OllieExtractionInstance]) {
  def train(instances: Iterable[ScoredOllieExtractionInstance]) = {
    val examples = instances.zipWithIndex map { case (scored, i) =>
      val vector = DenseVector(features.vectorize(scored.inst).toArray)
      Example[Boolean, DenseVector[Double]](scored.score, vector, id=i.toString)
    }

    val classifier = new LogisticClassifier.Trainer[Boolean,DenseVector[Double]].train(examples)

    (features.featureNames.iterator zip classifier.featureWeights.indexed(true).iterator) foreach { case (name, weight) => println(name + ": " + weight) }
  }
}