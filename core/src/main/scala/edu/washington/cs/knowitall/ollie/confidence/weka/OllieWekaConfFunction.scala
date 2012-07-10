package edu.washington.cs.knowitall.ollie.confidence.weka

import java.io.FileNotFoundException
import java.io.InputStream

import scala.Option.option2Iterable
import scala.util.Random

import edu.washington.cs.knowitall.ollie.confidence.FeatureSet
import edu.washington.cs.knowitall.ollie.confidence.OllieExtractionFeatures
import edu.washington.cs.knowitall.ollie.confidence.OllieIndependentConfFunction
import edu.washington.cs.knowitall.ollie.OllieExtractionInstance
import weka.classifiers.functions.Logistic
import weka.classifiers.Classifier
import weka.core.Instances

/** A wrapper for Weka's implementation of logistic regression. */
class OllieWekaConfFunction(
  val exampleSet: WekaExampleSet[OllieExtractionInstance],
  val classifier: Classifier) {

  def getConf(extr: OllieExtractionInstance): Double = {
    val instance = exampleSet.getWekaInstance(extr)

    classifier.distributionForInstance(instance)(0)
  }
}

object OllieWekaClassifierTrainer {
  val trainingResources = Seq("default-training.txt").map { name =>
    Option(OllieWekaClassifierTrainer.getClass.getResource(name)).getOrElse {
      throw new FileNotFoundException("Could not find training data resource: " + name)
    }
  }

  def loadDefaultClassifier(): OllieWekaConfFunction = {

    println("Loading classifier data...")
    val featureSet = new WekaExampleSet(new FeatureSet(OllieExtractionFeatures.getFeatures()))

    val dataInputs = trainingResources.map(_.openStream())
    val rawData = dataInputs.map(loadLabeledExtractions(_).toList)
    val trainingData = Random.shuffle(rawData.reduce(_ ++ _))
    dataInputs.foreach(_.close())

    val allTraining = Random.shuffle(trainingData)
    println("Training on %d examples".format(allTraining.size))

    trainClassifier(featureSet, allTraining)
  }

  def trainClassifier(
    featureSet: WekaExampleSet[OllieExtractionInstance],
    trainingExamples: Iterable[(OllieExtractionInstance, Int)]): OllieWekaConfFunction = {

    val instances = new Instances(featureSet.dataSet)

    trainingExamples.foreach {
      case (example, label) =>
        instances.add(featureSet.getLabeledWekaInstance(example, label))
    }

    val classifier = new Logistic()

    classifier.buildClassifier(instances)

    new OllieWekaConfFunction(featureSet, classifier)
  }

  def loadLabeledExtractions(inStream: InputStream): Iterator[(OllieExtractionInstance, Int)] = {
    val source = scala.io.Source.fromInputStream(inStream)

    val lines = source.getLines

    def splitOffLabel(line: String) = {
      val firstTab = line.indexWhere(_ == '\t')
      (line.substring(0, firstTab), line.substring(firstTab + 1, line.length()))
    }

    val pairs = lines.map(splitOffLabel(_))

    pairs.flatMap({
      case (rawLabel, rawExtr) =>
        OllieExtractionInstance.deserialize(rawExtr) match {
          case Some(extr) => {
            val label = if (rawLabel.contains("1")) 1 else 0
            Some(extr, label)
          }
          case None => None
        }
    })
  }

  // train the default classifier and write it to disk
  def main(args: Array[String]): Unit = {

    val defaultClassifier = OllieWekaClassifierTrainer.loadDefaultClassifier()

    System.out.println(defaultClassifier.classifier)
    
    val objOut = new java.io.ObjectOutputStream(new java.io.FileOutputStream("src/main/resources/default-classifier.model"))

    objOut.writeObject(defaultClassifier.classifier)

    objOut.flush()
    objOut.close()
  }
}