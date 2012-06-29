package edu.washington.cs.knowitall.ollie.confidence

import scala.collection.immutable.Map

class FeatureSet[T](val featureMap: Map[String, T => Double]) {
  def featureNames(): Iterable[String] =
    featureMap.keys.toList

  def numFeatures(): Int =
    featureNames.size

  def getFeatureVector(example: T): Iterable[Option[Double]] =
    featureNames.map({ name =>
      val featureFunction = featureMap(name)
      val featureValue = featureFunction(example)
      if (featureValue >= 0) Some(featureValue) else None
    }).toList

  def getPrintableFeatureVector(example: T): Iterable[String] =
    getFeatureVector(example).map({ opt =>
      if (opt.isDefined) opt.get.toString()
      else "None"
    })
}

object FeatureSet {
  val binaryClass = true

  def apply[T](featureMap: Map[String, T => Double]): FeatureSet[T] = {
    new FeatureSet(featureMap)
  }
}