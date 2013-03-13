package edu.knowitall.ollie

import scala.Option.option2Iterable
import scala.collection.breakOut

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.common.HashCodeHelper
import edu.knowitall.openparse.extract.Extraction.Part
import edu.knowitall.tool.parse.graph.DependencyNode

/** A base representation for additional context around an extraction. */
sealed abstract class Context {
  def text: String
  def interval: Interval
}

/** A representation for an enabling condition.
  * An example of an enabling condition is "if it's raining".
  */
case class EnablingCondition(
    /** The enabling condition word, i.e. "if" */
    val prefix: String,
    /** The rest of the enabling condition, i.e. "it's raining" */
    val phrase: String,
    /** The token interval of the enabling condition */
    override val interval: Interval) extends Context {
  override def text = prefix + " " + phrase

  def serialize: String = Seq(prefix, phrase, interval.start.toString, interval.last.toString).map(_.replaceAll("_", "_UNSC_")).mkString("_")
}

object EnablingCondition {
  def deserialize(string: String) = {
    val Array(prefix, phrase, intervalStart, intervalLast) = try (string.split("_"))
    catch {
      case e => throw new RuntimeException("could not deserialize EnablingCondition: " + string, e);
    }
    new EnablingCondition(prefix, phrase, Interval.closed(intervalStart.toInt, intervalLast.toInt))
  }
}

/** A representation for an attribution.
  * An example of an is "Obama believes".
  */
case class Attribution(
    /** The argument of the attribution, i.e. "Obama" */
    val arg: String,
    /** The token interval of the argument of the attribution */
    val argInterval: Interval,
    /** The relation of the attribution, i.e. "believes" */
    val rel: String,
    /** The token interval of the relation of the attribution */
    override val interval: Interval) extends Context {
  override def text = arg + " " + rel

  def serialize: String = {
    val fields = Seq(arg, rel, argInterval.start.toString, argInterval.last.toString, interval.start.toString, interval.last.toString)
    fields.map(_.replaceAll("_", "_UNSC_")).mkString("_")
  }
}

object Attribution {
  def deserialize(string: String) = {
    val Array(arg, rel, argIntervalStart, argIntervalLast, relIntervalStart, relIntervalLast) = try (string.split("_"))
    catch {
      case e => throw new RuntimeException("could not deserialize Attribution: " + string, e);
    }
    val argInterval = Interval.closed(argIntervalStart.toInt, argIntervalLast.toInt)
    val relInterval = Interval.closed(relIntervalStart.toInt, relIntervalLast.toInt)

    new Attribution(arg, argInterval, rel, relInterval)
  }
}

/** A representation of an Ollie extraction, i.e. we could get the following
  * extraction from the example sentence.
  *
  * {{{
  * When I'm dreaming David Bowie sings that Ziggy sucked up into his mind.
  * (Ziggy, sucked up, into his mind)[attribution = "David Bowie")
  * }}}
  */
class OllieExtraction(
  /** The first argument (subject) of the extraction, i.e. "Ziggy" */
  val arg1: Part,
  /** The relation of the extraction, i.e. "sucked up" */
  val rel: Part,
  /** The second argument (object) of the extraction, i.e. "into his mind" */
  val arg2: Part,
  /** The confidence value from OpenParse. */
  private[ollie] val openparseConfidence: Double,
  /** The enabling condition, if any.  I.e. "When I'm dreaming" */
  val enabler: Option[EnablingCondition],
  /** The attribution, if any.  I.e. "David Bowie sings that" */
  val attribution: Option[Attribution]) {

  import OllieExtraction.{serializePart, deserializePart}

  override def equals(that: Any) = that match {
    case that: OllieExtraction =>
      this.arg1 == that.arg1 &&
      this.rel == that.rel &&
      this.arg2 == that.arg2 &&
      this.enabler == that.enabler &&
      this.attribution == that.attribution &&
      this.openparseConfidence == that.openparseConfidence
    case _ => false
  }

  override def hashCode = HashCodeHelper(
      this.arg1,
      this.rel,
      this.arg2,
      this.enabler,
      this.attribution,
      this.openparseConfidence)

  def tabSerialize: String = {
    val enablerString = enabler match {
      case Some(enablingCondition) => enablingCondition.serialize
      case None => "None"
    }
    val attrString = attribution match {
      case Some(attr) => attr.serialize
      case None => "None"
    }

    val fieldStrings = Seq(arg1, rel, arg2).map(serializePart(_)) ++ Seq("%.05f".format(openparseConfidence), enablerString, attrString)
    fieldStrings.map(_.replaceAll("\t", "_TAB_")).mkString("\t")
  }

  /** The full text of this extraction. */
  def text = Iterable(arg1.text, rel.text, arg2.text).mkString(" ")

  /** All the nodes in this extraction. */
  def nodes = arg1.nodes ++ rel.nodes ++ arg2.nodes

  /** The spanning interval of the nodes in this extraction. */
  def span = Interval.span(nodes.map(_.indices))

  override def toString = {
    val extentions = Iterable(
        enabler.map("enabler="+_.text),
        attribution.map("attrib="+_.text)).flatten match {
      case Nil => ""
      case list => list.mkString("[", ";", "]")
    }
    "(%s; %s; %s)".format(arg1.text, rel.text, arg2.text) + extentions
  }
}

object OllieExtraction {
  def tabDelimitedColumns = Seq("Arg1Part", "RelPart", "Arg2Part", "Confidence", "Enabler", "Attribution").mkString("\t")

  def tabDeserialize(array: Seq[String]): (OllieExtraction, Seq[String]) = {
    array match {
      case Seq(arg1Part, relPart, arg2Part, openparseConfString, enablerString, attrString, rest @ _*) => {
        val parts = Seq(arg1Part, relPart, arg2Part) map deserializePart
        val enabler = if (enablerString.equals("None")) None else Some(EnablingCondition.deserialize(enablerString))
        val attribution = if (attrString.equals("None")) None else Some(Attribution.deserialize(attrString))
        val extr = new OllieExtraction(parts(0), parts(1), parts(2), openparseConfString.toDouble, enabler, attribution)
        (extr, rest)
      }
    }
  }

  def tabDeserialize(s: String): OllieExtraction = {
    val (extr, rest) = tabDeserialize(s.split("\t"))
    require(rest.isEmpty)
    extr
  }

  def serializePart(part: Part): String = {
    val serializedNodes = part.nodes.iterator.map(_.serialize).mkString("; ")
    Iterable(part.text, serializedNodes).mkString(" ;;; ")
  }

  def deserializePart(string: String): Part = {
    val Array(partText, partNodes) = try (string.split("\\s*;;;\\s*"))
      catch {
        case e => throw new RuntimeException("could not deserialize Extraction.Part: " + string, e);
      }

    val nodesSortedSet: scala.collection.SortedSet[DependencyNode] =
      try (partNodes.split("\\s*;\\s*").map(DependencyNode.deserialize(_))(breakOut))
      catch {
        case e => throw new RuntimeException("could not deserialize Extraction.Part: " + string, e);
      }

    new Part(nodesSortedSet, partText)
  }
}

