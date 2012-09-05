package edu.washington.cs.knowitall.ollie.confidence

import java.io.InputStream
import java.net.URL
import java.util.Scanner
import scala.collection.mutable
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.ollie.OllieExtractionInstance
import java.io.PrintWriter
import java.io.File
import edu.washington.cs.knowitall.common.Resource
import scala.io.Source
import java.io.FileOutputStream

/** An implementation of logistic regression of features that can be
  * represented as a double. */
class OllieIndependentConfFunction(
  val featureSet: FeatureSet[OllieExtractionInstance],
  val featureWeights: Map[String, Double],
  val intercept: Double) {

  def this(featureSet: FeatureSet[OllieExtractionInstance], weights: Map[String, Double]) = {
    this(featureSet, weights, weights("Intercept"))
  }

  def apply(extraction: OllieExtractionInstance): Double = getConf(extraction)

  def getConf(extraction: OllieExtractionInstance): Double = {
    var z = this.featureSet.featureNames.map { name =>
      val weight = featureWeights(name)
      weight * featureSet.featureMap(name).apply(extraction)
    } sum

    1.0 / (1.0 + math.exp(-(z + this.intercept)))
  }

  def save(writer: PrintWriter) = {
    for ((name, weight) <- featureWeights) {
      writer.println(name + "\t" + weight)
    }

    println("Intercept" + "\t" + intercept)
  }

  def saveFile(file: File) = {
    Resource.using(new PrintWriter(file, "UTF8")) { writer =>
      this.save(writer)
    }
  }
}

object OllieIndependentConfFunction {
  val logger = LoggerFactory.getLogger(classOf[OllieIndependentConfFunction])

  def loadDefaultClassifier(): OllieIndependentConfFunction = {
    val featureSet = OllieFeatureSet

    val resourceUrl = Option(this.getClass.getResource("default-classifier.txt")).getOrElse {
      throw new IllegalArgumentException("Could not load confidence function resource.")
    }

    fromUrl(featureSet, resourceUrl)
  }

  def fromUrl(featureSet: FeatureSet[OllieExtractionInstance], url: URL) = {
    using(url.openStream) { input =>
      new OllieIndependentConfFunction(featureSet, buildFeatureWeightMap(input))
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
