package edu.washington.cs.knowitall.ollie

import scala.Option.option2Iterable
import scala.collection.SortedSet
import scala.collection.immutable
import edu.washington.cs.knowitall.collection.immutable.graph.Graph
import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.openparse.extract.DetailedExtraction
import edu.washington.cs.knowitall.openparse.extract.Extraction
import edu.washington.cs.knowitall.openparse.extract.Extraction.AdverbialModifier
import edu.washington.cs.knowitall.openparse.extract.Extraction.ClausalComponent
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.postag.Postagger
import edu.washington.cs.knowitall.openparse.extract.Extraction.Part

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
  val preposition: Option[String],
  val contents: String,
  nodes: SortedSet[DependencyNode],
  val connections: Set[Graph.Edge[DependencyNode]])
  extends Extraction.Part(nodes, preposition.map(_ + " ").getOrElse("") + contents) {

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
  preposition: Option[String],
  contents: String,
  nodes: SortedSet[DependencyNode],
  connections: Set[Graph.Edge[DependencyNode]],
  val annotation: String)
  extends Suffix(preposition, contents, nodes, connections) {
  def this(suffix: Suffix, annotation: String) =
    this(suffix.preposition, suffix.contents, suffix.nodes, suffix.connections, annotation)
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

  def dobjs = suffixes.filter(suffix => suffix.connections.size == 1 && suffix.connections.head.label == "dobj")
  def iobjs = suffixes.filter(suffix => suffix.connections.size == 1 && suffix.connections.head.label == "iobj")
}

object NaryExtraction {
  implicit object SuffixOrdering extends Ordering[Suffix] {
    def compare(x: Suffix, y: Suffix) = x.span.compare(y.span)
  }

  /**
   * Create extended extractions from a collection of extractions
   * from the same sentence.
   */
  def from(extrs: Iterable[OllieExtractionInstance]): Iterable[(NaryExtraction, Map[Suffix, OllieExtractionInstance])] = {
    // break off the preposition
    case class BrokenExtraction(rel: String, preposition: Option[String], extr: OllieExtractionInstance)
    val split: Iterable[BrokenExtraction] = extrs.map {
      inst =>
        Postagger.prepositions.filter(prep => inst.extr.rel.text endsWith (" " + prep)) match {
          case preps if preps.isEmpty => BrokenExtraction(inst.extr.rel.text, None, inst)
          case preps =>
            val longest = preps.maxBy(_.length)
            BrokenExtraction(inst.extr.rel.text.dropRight(longest.length + 1), Some(longest), inst)
        }

    }

    // group by the arg1 and text
    split groupBy {
      case BrokenExtraction(rel, preposition, inst) =>
        (inst.extr.arg1.text, rel)
    } filter (_._2.size > 1) map {
      case ((arg1, rel), extrs) =>
        val suffixes = extrs.map {
          case extr @ BrokenExtraction(rel, prep, inst) =>
            new Suffix(prep, inst.extr.arg2.text, inst.extr.arg2.nodes, Part.connections(inst.`match`, inst.extr.arg2.nodes)) -> extr.extr
        }

        val first = extrs.head.extr.extr
        val argument1 = new Extraction.Part(first.arg1.nodes, arg1)
        val relation = new Extraction.Part(first.rel.nodes, rel)

        val attributions = extrs.flatMap(_.extr.extr.attribution).toSet.toSeq
        val enablers = extrs.flatMap(_.extr.extr.enabler).toSet.toSeq

        (new NaryExtraction(argument1, relation, suffixes.map(_._1).toSeq, enablers = enablers, attributions = attributions), suffixes.toMap)
    }
  }

  def triples(naryExtrs: Iterable[(NaryExtraction, Map[Suffix, OllieExtractionInstance])]) = {
    for {
      (nary, map) <- naryExtrs
      suffix <- nary.suffixes
      val inst = map(suffix)
    } yield {
      def f[T](list: Iterable[T]): Seq[Option[T]] = {
        list match {
          case _ if list.isEmpty => Seq(None)
          case _ => list.map(Some(_)).toSeq
        }
      }
      val dobjs = f(nary.dobjs filter (_ != suffix))
      val iobjs = f(nary.iobjs filter (_ != suffix))
      val firstPrep =
        if (nary.dobjs.isEmpty && nary.iobjs.isEmpty)
          f(nary.suffixes.find(_.preposition.isDefined) filter (_ != suffix))
        else List(None)

      for {
        dobj <- dobjs
        iobj <- iobjs
        prep <- firstPrep
      } yield {
        val additions = (dobj ++ iobj ++ prep).toSeq.sortBy(_.span)
        val relText = nary.rel.text + (if (additions.size > 0) " " else "") + additions.map(_.text).mkString(" ") + suffix.preposition.map(" " + _).getOrElse("")
        val relNodes = inst.extr.rel.nodes ++ additions.flatMap(_.nodes)

        val extr = inst.extr.withRelation(new Part(relNodes, relText))
        new OllieExtractionInstance(extr, inst.sent, inst.`match`, inst.pat)
      }
    }
  }
}