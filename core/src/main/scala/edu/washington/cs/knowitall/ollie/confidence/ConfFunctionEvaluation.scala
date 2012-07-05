package edu.washington.cs.knowitall.ollie.confidence

import scala.util.Random

import edu.washington.cs.knowitall.common.Analysis
import edu.washington.cs.knowitall.ollie.confidence.weka.WekaExampleSet
import edu.washington.cs.knowitall.ollie.confidence.weka.OllieWekaClassifierTrainer
import edu.washington.cs.knowitall.ollie.confidence.weka.OllieWekaConfFunction
import edu.washington.cs.knowitall.ollie.OllieExtraction
import edu.washington.cs.knowitall.ollie.OllieExtractionInstance

object ConfFunctionEvaluation {

  val NUM_TRIALS = 1
  
  val trainingFraction = 0.998

  def infoPrint(str: String): Unit = System.err.println(str)
  
  def main(args: Array[String]) {

    infoPrint("Building Feature Set...")

    val featureSet = FeatureSet(OllieExtractionFeatures.getFeatures())
    val exampleSet = new WekaExampleSet(featureSet)
    infoPrint("Loading Labeled Training Examples...")
    val dataInputs = OllieWekaClassifierTrainer.trainingResources.map(_.openStream())
    val rawData = dataInputs.flatMap(OllieWekaClassifierTrainer.loadLabeledExtractions(_))
    

    val allData = Random.shuffle(rawData.toList)
    dataInputs.foreach(_.close())

    infoPrint("Development Data Size:" + allData.size)

    val allTrials = (1 to NUM_TRIALS).map(_ => trial(allData, exampleSet))
    val trainingRuns = allTrials.map(_._1)
    val testingRuns = allTrials.map(_._2)
    // Get data for Average Precision/Precision Variance over the multiple trials
    val trainingApvy = getApvy(trainingRuns.map(_.map(_._2 == 1)))
    val testingApvy = getApvy(testingRuns.map(_.map(_._2 == 1)))

    // compute max yields so we can normalize yield to recall
    val maxTrainYield = trainingApvy.map(_._1).reduce(scala.math.max(_, _)).toDouble
    val maxTestYield = testingApvy.map(_._1).reduce(scala.math.max(_, _)).toDouble

    // print training examples
    println("\n\nAvgConf\tLabel\t" + evalTabDelimitedColumns + "\t" + featureSet.featureNames().mkString("\t"))
    trainingRuns.flatten.toList.groupBy(_._1).iterator.map({ tuple =>
      val inst = tuple._1
      val label = tuple._2.head._2
      val confs = tuple._2.map(_._3)
      val avgConf = confs.reduce(_ + _).toDouble / confs.size
      val featureVector = featureSet.getPrintableFeatureVector(inst).mkString("\t")
      (avgConf, label, evalTabDelimited(inst), featureVector)
    }).toList.sortBy(-_._1).foreach({ tuple =>
      println("%s\t%s\t%s\t%s".format(tuple._1, tuple._2, tuple._3, tuple._4))
    })

    
    def printPrecRecall = {
      // print training and test performance (side by side, zipped)
      val trainOutput = trainingApvy.map(t => "%s\t%s\t%s".format(t._1 / maxTrainYield, t._2, t._3))
      val testOutput = testingApvy.map(t => "%s\t%s\t%s".format(t._1 / maxTestYield, t._2, t._3))
      println("Test Yield\tTest Avg Prec\tTest PrecVar\tTrain Yield\tTrain Avg Prec\tTrain PrecVar")
      trainOutput.zipAll(testOutput, "", "").toList.foreach(p => println("%s\t%s".format(p._1, p._2)))
    }
    
    printPrecRecall
  }
  
  def evalTabDelimitedColumns: String = {
    Seq(
      "attrib",
      "enabler",
      "arg1Text",
      "relText",
      "arg2Text",
      "sentence"
        ).mkString("\t")
  }
  
  def evalTabDelimited(inst: OllieExtractionInstance): String = {
    val extr = inst.extr
    val sent = inst.sent.nodes.iterator.map(_.string)
    val fields = Seq(
      extr.attribution,
      extr.enabler,
      extr.arg1.text,
      extr.rel.text,
      extr.arg2.text,
      sent.mkString(" ")
    )
    fields.mkString("\t")
  }

  // randomly split the data into training/test sets, and provide each set in decreasing order of confidence
  def trial(allData: List[(OllieExtractionInstance, Int)], featureSet: WekaExampleSet[OllieExtractionInstance]): (List[(OllieExtractionInstance, Int, Double)], List[(OllieExtractionInstance, Int, Double)]) = {

    val shuffledData = Random.shuffle(allData)
    val splitIndex = (shuffledData.length * trainingFraction).toInt
    val trainingData = shuffledData.take(splitIndex)
    val testingData = shuffledData.drop(splitIndex)
    System.err.println("Train Size: %s Test Size: %s".format(trainingData.size, testingData.size))
    
    val confFunction = OllieWekaClassifierTrainer.loadDefaultClassifier
    
    infoPrint(confFunction.classifier.toString)

    val confedTraining = trainingData.map { case (extr, label) => (extr, label, confFunction.getConf(extr)) }
    val confedTesting = testingData.map { case (extr, label) => (extr, label, confFunction.getConf(extr)) }

    val sortedTraining = confedTraining.sortBy(-_._3)
    val sortedTesting = confedTesting.sortBy(-_._3)

    (sortedTraining, sortedTesting)
  }

  def getPrecisionYield(extrLabelConfs: List[(OllieExtraction, Int, Double)]): List[(Int, Double)] = {

    var correctSoFar = 0
    var totalSoFar = 0.0
    var returnList: List[(Double, Double)] = Nil

    for ((extr, label, conf) <- extrLabelConfs) yield {
      if (label == 1) correctSoFar += 1
      totalSoFar += 1.0
      val precision = correctSoFar / totalSoFar
      val yld = correctSoFar
      (yld, precision)
    }
  }
  
  // Group by confs
  // filter groups both having labels 1 and 0 in them
  // toString them
  // return
  def getConflictErrors(extrLabelConfs: List[(OllieExtraction, Int, Double)]): Iterable[(Double, Iterable[(OllieExtraction, Int, Double)])] = {

    extrLabelConfs.groupBy(t => t._3).filter(l => l._2.exists(_._2 == 1) && l._2.exists(_._2 == 0)).toList.sortBy(-_._2.length)
  }

  // Average Precision/Precision Variance/Yield
  def getApvy(trials: Seq[Seq[Boolean]]): Seq[(Int, Double, Double)] = {

    // (yield, List[precision] => yield, avg. prec, prec std. dev)
    def toMeanVariance(point: (Int, Seq[(Int, Double)])): (Int, Double, Double) = {
      val mean = point._2.map(_._2).reduce(_ + _).toDouble / point._2.size
      val variance = point._2.map(v => (v._2 - mean) * (v._2 - mean)).reduce(_ + _)
      (point._1, mean, scala.math.sqrt(variance))
    }

    trials.flatMap(Analysis.precisionYield(_)).groupBy(_._1).iterator.toSeq.map(toMeanVariance(_)).sortBy(_._1)
  }
}
