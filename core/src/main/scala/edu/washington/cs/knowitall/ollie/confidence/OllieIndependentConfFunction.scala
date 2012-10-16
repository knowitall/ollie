package edu.washington.cs.knowitall.ollie.confidence

import java.io.InputStream
import java.net.URL
import java.util.Scanner

import scala.collection.mutable

import org.slf4j.LoggerFactory

import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.ollie.OllieExtractionInstance
import edu.washington.cs.knowitall.tool.conf.FeatureSet
import edu.washington.cs.knowitall.tool.conf.impl.LogisticRegression

/** An implementation of logistic regression of features that can be
  * represented as a double. */

object OllieIndependentConfFunction {
  val logger = LoggerFactory.getLogger(classOf[OllieIndependentConfFunction])
  
  type OllieIndependentConfFunction = LogisticRegression[OllieExtractionInstance]

  val defaultModelUrl = Option(this.getClass.getResource("default-classifier.txt")).getOrElse {
    throw new IllegalArgumentException("Could not load confidence function resource.")
  }

  def loadDefaultClassifier(): OllieIndependentConfFunction = {
    fromUrl(OllieFeatureSet, defaultModelUrl)
  }

  def fromUrl(featureSet: FeatureSet[OllieExtractionInstance, Double], url: URL) = {
    using(url.openStream) { input =>
      new LogisticRegression[OllieExtractionInstance](featureSet, buildFeatureWeightMap(input))
    }
  }

  private val tab = """\t""".r
  def buildFeatureWeightMap(input: InputStream): Map[String, Double] = {
    val featureWeights = new mutable.HashMap[String, Double]()
    val scan = new Scanner(input, "UTF8")

    var numFeatures = 0

    while (scan.hasNextLine()) {
      numFeatures += 1
      val line = scan.nextLine()
      val parts = tab.split(line)
      val featureName = parts(0).trim
      val weight = parts(1).toDouble
      featureWeights.put(featureName, weight)
    }

    logger.debug("Confidence features loaded: " + numFeatures)

    featureWeights.toMap
  }
}
