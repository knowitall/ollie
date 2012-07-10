package edu.washington.cs.knowitall.ollie.confidence.weka

import weka.core.Instances
import weka.core.Instance
import weka.core.FastVector
import weka.core.Attribute

import edu.washington.cs.knowitall.ollie.confidence.FeatureSet
import weka.core.FastVector

/** Represents a set of instances for a Weka classifier.
  *
  * T is the input object (e.g. a sentence). The implementation
  * assumes that Double is sufficient as a return type. Some weka classifiers
  * cannot handle "numeric attributes", however.
  *
  * Assumes that the class attribute should be binary (e.g. positive and negative)
  *
  * Features that return a value outside of [0..1] are treated as missing attributes
  * (e.g. return -1 to indicate None) TODO(?): refactor all features to return Option[Double]
  */
class WekaExampleSet[T](val features: FeatureSet[T]) {
  private val attributes: FastVector = new FastVector(features.featureMap.size + 1)

  features.featureMap.keys.foreach(featureName => attributes.addElement(new Attribute(featureName)))

  // add the classes to the attribute
  {
    val classVals = new FastVector(2)
    classVals.addElement("positive")
    classVals.addElement("negative")

    val classAttribute = new Attribute("class", classVals)
    attributes.addElement(classAttribute)
  }

  val dataSet = new Instances("Weka Instances Object", attributes, 0)
  dataSet.setClassIndex(attributes.size() - 1)

  private val attributeList = (0 to attributes.size() - 1).map(attributes.elementAt(_).asInstanceOf[Attribute])

  def getWekaInstance(example: T): Instance = {
    val inst = new Instance(features.numFeatures + 1)

    val featureVector = features.getFeatureVector(example)
    val attrValuePairs = attributeList.zip(featureVector)

    attrValuePairs.foreach {
      case (attr, valueOpt) =>
        valueOpt match {
          case Some(value) => inst.setValue(attr, value)
          case None =>
        }
    }

    inst.setDataset(dataSet)

    inst
  }

  def getLabeledWekaInstance(example: T, label: Int): Instance = {

    val inst = getWekaInstance(example)

    inst.setValue(attributeList.last, 1 - label)

    inst
  }
}

object WekaExampleSet {
  class EnrichedFastVector(fv: FastVector) extends Iterable[Attribute] {
    override def iterator = new Iterator[Attribute] {
      var index = 0

      def hasNext = index < fv.size
      def next = {
        val item = fv.elementAt(index)
        index += 1
        item.asInstanceOf[Attribute]
      }
    }
  }

  implicit def enrichFastVector(fv: FastVector) = new EnrichedFastVector(fv)
}