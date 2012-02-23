package edu.washington.cs.knowitall.pattern.extract

import scala.collection.immutable
import edu.washington.cs.knowitall.tool.postag.PosTagger
import edu.washington.cs.knowitall.collection.immutable.Interval

class ExtractionPart(val string: String, val interval: Interval) extends Ordered[ExtractionPart] {
  override def compare(that: ExtractionPart) = 
    this.interval.compare(that.interval)
    
  override def toString = string.replaceAll("/", "")
}

class Suffix(
    string: String,
    interval: Interval,
    val confidence: Double)
extends ExtractionPart(string, interval) {
  override def toString = ("%1.4f" format confidence) + "/\"" + super.toString + "\""
  
  def annotate(string: String) = 
    new AnnotatedSuffix(this, string)
}

class AnnotatedSuffix(
    string: String,
    interval: Interval,
    confidence: Double,
    val annotation: String)
extends Suffix(string, interval, confidence) {
  def this(suffix: Suffix, annotation: String) = 
    this(suffix.string, suffix.interval, suffix.confidence, annotation)
  override def toString = annotation + ": " + super.toString
}

class ExtendedExtraction(val arg1: ExtractionPart, val rel: ExtractionPart, val suffixes: Seq[Suffix]) {
  override def toString =
    "(" + arg1.string + ", " + rel.string + ", " + suffixes.map(_.string).mkString(", ") + ")"
}

object ExtendedExtraction {
  implicit object SuffixOrdering extends Ordering[Suffix] { 
    def compare(x: Suffix, y: Suffix) = x.interval.compare(y.interval)
  }
  
  def from(extrs: Seq[(Double, DetailedExtraction)]) = {
    // keep extractions that end with a one-word preposition
    val prepositionEnding = extrs.filter { case (conf, extr) =>
      PosTagger.simplePrepositions(extr.rel drop (1 + extr.rel lastIndexOf ' '))
    }
      
    // break off the preposition
    val split: Seq[(String, String, (Double, DetailedExtraction))] = prepositionEnding.map { case (conf, extr) =>
      val preps = PosTagger.prepositions.filter(extr.rel endsWith _)
      val longest = preps.maxBy(_.length)
      (extr.rel.dropRight(longest.length + 1), longest, (conf, extr))
    }
    
    split groupBy { case (rel, longest, (conf, extr)) =>
      (extr.arg1, rel)
    } filter (_._2.size > 1) map { case ((arg1, rel), extrs) =>
      val suffixes: immutable.SortedSet[Suffix] = extrs.map { case (rel, prep, (conf, extr)) =>
        new Suffix(prep + " " + extr.arg2, Interval.span(extr.arg2Nodes.map(_.indices)), conf)
      }(scala.collection.breakOut)
      
      val first = extrs.head._3._2
      val argument1 = new ExtractionPart(arg1, Interval.span(first.arg1Nodes.map(_.indices)))
      val relation = new ExtractionPart(rel, Interval.span(first.relNodes.map(_.indices)))
      
      new ExtendedExtraction(argument1, relation, suffixes.toSeq)
    }
  }
}