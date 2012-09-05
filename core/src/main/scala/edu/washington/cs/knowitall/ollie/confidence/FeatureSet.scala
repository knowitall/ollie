package edu.washington.cs.knowitall.ollie.confidence

import scala.collection.immutable.Map

/** FeatureSet represents a set of features on T that can be
  * represented as a double */
class FeatureSet[T](val featureMap: Map[String, T => Double]) {
  def apply(name: String) = featureMap(name)

  def featureNames(): Seq[String] =
    featureMap.keys.toSeq

  def numFeatures(): Int =
    featureNames.size

  def vectorize(example: T): Seq[Double] =
    featureNames.map({ name =>
      val featureFunction = featureMap(name)
      featureFunction(example)
    })(scala.collection.breakOut)
}

object FeatureSet {
  val binaryClass = true

  def apply[T](featureMap: Map[String, T => Double]): FeatureSet[T] = {
    new FeatureSet(featureMap)
  }
}
