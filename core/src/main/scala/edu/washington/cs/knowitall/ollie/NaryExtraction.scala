package edu.washington.cs.knowitall.ollie

import scala.Option.option2Iterable
import scala.collection.SortedSet
import scala.collection.immutable

import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.openparse.extract.DetailedExtraction
import edu.washington.cs.knowitall.openparse.extract.Extraction
import edu.washington.cs.knowitall.openparse.extract.Extraction.AdverbialModifier
import edu.washington.cs.knowitall.openparse.extract.Extraction.ClausalComponent
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.postag.Postagger

/**
 * Represents a part {arg1, rel, arg2} of an extraction.
 *
 * @param  string  the representation of the part
 * @param  interval  the interval of the part in the source sentence
 */
class ExtractionPart(val string: String, val interval: Interval) extends Ordered[ExtractionPart] {
  override def compare(that: ExtractionPart) =
    this.interval compare that.interval

  override def toString = string.replaceAll("/", "")
}

/**
 * Represents a possible suffix for an extended extraction.
 * For example, in the sentence "He ate from 7 until 10."
 * there are two suffixes: "from 7" and "until 10".
 *
 * @param  string  the text of the suffix
 * @param  interval  the interval of the suffix in the source sentence
 * @param  confidence  the confidence of the suffix
 */
class Suffix(
  text: String,
  nodes: SortedSet[DependencyNode],
  val confidence: Double)
  extends Extraction.Part(nodes, text) {
  override def toString = ("%1.4f" format confidence) + "/\"" + super.toString + "\""

  /** Annote the suffix with a type. */
  def annotate(string: String) =
    new AnnotatedSuffix(this, string)
}

/**
 * Represents a possible suffix for an extended extraction
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
  text: String,
  nodes: SortedSet[DependencyNode],
  confidence: Double,
  val annotation: String)
  extends Suffix(text, nodes, confidence) {
  def this(suffix: Suffix, annotation: String) =
    this(suffix.text, suffix.nodes, suffix.confidence, annotation)
  override def toString = annotation + "/" + super.toString
}

/**
 * A representaiton of an n-ary extraction, i.e.
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
class NaryExtraction(val arg1: Extraction.Part, val rel: Extraction.Part, val suffixes: Seq[Suffix], val attributions: Seq[Attribution] = Seq.empty, val enablers: Seq[EnablingCondition] = Seq.empty) {
  override def toString =
    "(" + arg1.text + ", " + rel.text + ", " + suffixes.map(_.text).mkString(", ") + ")"
}

object NaryExtraction {
  implicit object SuffixOrdering extends Ordering[Suffix] {
    def compare(x: Suffix, y: Suffix) = x.span.compare(y.span)
  }

  /**
   * Create extended extractions from a collection of extractions
   * from the same sentence.
   */
  def from(extrs: Iterable[(Double, OllieExtractionInstance)]): Iterable[NaryExtraction] = {
    // keep extractions that end with a one-word preposition
    val prepositionEnding = extrs.filter {
      case (conf, inst) =>
        Postagger.simplePrepositions(inst.extr.rel.text drop (1 + inst.extr.rel.text lastIndexOf ' '))
    }

    // break off the preposition
    case class BrokenExtraction(rel: String, preposition: String, extr: (Double, OllieExtraction))
    val split: Iterable[BrokenExtraction] = prepositionEnding.map {
      case (conf, inst) =>
        val preps = Postagger.prepositions.filter(inst.extr.rel.text endsWith _)
        val longest = preps.maxBy(_.length)
        BrokenExtraction(inst.extr.rel.text.dropRight(longest.length + 1), longest, (conf, inst.extr))
    }

    // group by the arg1 and text
    split groupBy {
      case BrokenExtraction(rel, preposition, (conf, extr)) =>
        (extr.arg1.text, rel)
    } filter (_._2.size > 1) map {
      case ((arg1, rel), extrs) =>
        val suffixes: immutable.SortedSet[Suffix] = extrs.map {
          case BrokenExtraction(rel, prep, (conf, extr)) =>
            new Suffix(prep + " " + extr.arg2.text, extr.arg2.nodes, conf)
        }(scala.collection.breakOut)

        val first = extrs.head.extr._2
        val argument1 = new Extraction.Part(first.arg1.nodes, arg1)
        val relation = new Extraction.Part(first.rel.nodes, rel)

        val attributions = extrs.flatMap(_.extr._2.attribution).toSet.toSeq
        val enablers = extrs.flatMap(_.extr._2.enabler).toSet.toSeq

        new NaryExtraction(argument1, relation, suffixes.toSeq, enablers = enablers, attributions = attributions)
    }
  }
}
