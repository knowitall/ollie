package edu.washington.cs.knowitall.openparse.extract

import scala.collection.immutable

import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.openparse.extract.Extraction.{ClausalComponent, AdverbialModifier}
import edu.washington.cs.knowitall.tool.postag.Postagger

/** Represents a part {arg1, rel, arg2} of an extraction.
  *
  * @param  string  the representation of the part
  * @param  interval  the interval of the part in the source sentence
  */
class ExtractionPart(val string: String, val interval: Interval) extends Ordered[ExtractionPart] {
  override def compare(that: ExtractionPart) =
    this.interval.compare(that.interval)

  override def toString = string.replaceAll("/", "")
}

/** Represents a possible suffix for an extended extraction.
  * For example, in the sentence "He ate from 7 until 10."
  * there are two suffixes: "from 7" and "until 10".
  *
  * @param  string  the text of the suffix
  * @param  interval  the interval of the suffix in the source sentence
  * @param  confidence  the confidence of the suffix
 */
class Suffix(
    string: String,
    interval: Interval,
    val confidence: Double)
extends ExtractionPart(string, interval) {
  override def toString = ("%1.4f" format confidence) + "/\"" + super.toString + "\""

  /** Annote the suffix with a type. */
  def annotate(string: String) =
    new AnnotatedSuffix(this, string)
}

/** Represents a possible suffix for an extended extraction
  * along with an annotation.
  *
  * For example, in the sentence "He ate from 7 until 10."
  * there are two suffixes: "from 7" and "until 10".
  *
  * @param  string  the text of the suffix
  * @param  interval  the interval of the suffix in the source sentence
  * @param  confidence  the confidence of the suffix
  * @param  annotation  an annotation for the suffix
 */
class AnnotatedSuffix(
    string: String,
    interval: Interval,
    confidence: Double,
    val annotation: String)
extends Suffix(string, interval, confidence) {
  def this(suffix: Suffix, annotation: String) =
    this(suffix.string, suffix.interval, suffix.confidence, annotation)
  override def toString = annotation + "/" + super.toString
}

/** A representaiton of an n-ary extraction, i.e.
  *
  *   (Michael, ran, to the store, on Monday, at 2 PM)
  *
  * N-ary extractions have multiple secondary arguments (objects)
  * and these arguments include the preposition.
  *
  * @param  arg1  the first argument
  * @param  rel  the relation
  * @param  suffixes  the suffixes
  * @param  clausals  a clause restricting this extraction to a context
  * @param  modifier  a modifier for this extraction (i.e. attribution)
  *
  * @author Michael Schmitz
  */
class ExtendedExtraction(val arg1: ExtractionPart, val rel: ExtractionPart, val suffixes: Seq[Suffix], val clausals: Seq[ClausalComponent] = Seq.empty, val modifiers: Seq[AdverbialModifier] = Seq.empty) {
  override def toString =
    "(" + arg1.string + ", " + rel.string + ", " + suffixes.map(_.string).mkString(", ") + ")"
}

object ExtendedExtraction {
  implicit object SuffixOrdering extends Ordering[Suffix] {
    def compare(x: Suffix, y: Suffix) = x.interval.compare(y.interval)
  }

  /** Create extended extractions from a collection of extractions
    * from the same sentence. */
  def from(extrs: Seq[(Double, DetailedExtraction)]) = {
    // keep extractions that end with a one-word preposition
    val prepositionEnding = extrs.filter { case (conf, extr) =>
      Postagger.simplePrepositions(extr.rel.text drop (1 + extr.rel.text lastIndexOf ' '))
    }

    // break off the preposition
    val split: Seq[(String, String, (Double, DetailedExtraction))] = prepositionEnding.map { case (conf, extr) =>
      val preps = Postagger.prepositions.filter(extr.rel.text endsWith _)
      val longest = preps.maxBy(_.length)
      (extr.rel.text.dropRight(longest.length + 1), longest, (conf, extr))
    }

    split groupBy { case (rel, longest, (conf, extr)) =>
      (extr.arg1.text, rel)
    } filter (_._2.size > 1) map { case ((arg1, rel), extrs) =>
      val suffixes: immutable.SortedSet[Suffix] = extrs.map { case (rel, prep, (conf, extr)) =>
        new Suffix(prep + " " + extr.arg2.text, Interval.span(extr.arg2.nodes.map(_.indices)), conf)
      }(scala.collection.breakOut)

      val first = extrs.head._3._2
      val argument1 = new ExtractionPart(arg1, Interval.span(first.arg1.nodes.map(_.indices)))
      val relation = new ExtractionPart(rel, Interval.span(first.rel.nodes.map(_.indices)))

      val modifiers = extrs.flatMap(_._3._2.modifier).toSet.toSeq
      val clausals = extrs.flatMap(_._3._2.clausal).toSet.toSeq

      new ExtendedExtraction(argument1, relation, suffixes.toSeq, modifiers=modifiers, clausals=clausals)
    }
  }
}