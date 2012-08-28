package edu.washington.cs.knowitall.ollie

import scala.Array.canBuildFrom
import scala.collection.breakOut
import OllieExtraction.serializePart
import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.openparse.extract.Extraction.Part
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.collection.immutable.graph.pattern.Match
import edu.washington.cs.knowitall.openparse.extract.PatternExtractor

/** A base representation for additional context around an extraction. */
sealed abstract class Context {
  def text: String
  def interval: Interval
}

/** A representation for an enabling condition.
  * An example of an enabling condition is "if it's raining".
  */
class EnablingCondition(
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
class Attribution(
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

  def serialize: String = {
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

  def text = Iterable(arg1.text, rel.text, arg2.text).mkString(" ")

  def nodes = arg1.nodes ++ rel.nodes ++ arg2.nodes

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

class DetailedOllieExtraction(
  arg1: Part,
  rel: Part,
  arg2: Part,
  override val openparseConfidence: Double,
  enabler: Option[EnablingCondition],
  attribution: Option[Attribution],
  val `match`: Match[DependencyNode],
  val extractor: PatternExtractor)
extends OllieExtraction(arg1, rel, arg2, openparseConfidence, enabler, attribution)

object OllieExtraction {
  def tabDelimitedColumns = Seq("Arg1Part", "RelPart", "Arg2Part", "Confidence", "Enabler", "Attribution").mkString("\t")

  def deserialize(s: String): Option[OllieExtraction] = {
    def error = { System.err.println("Couldn't deserialize: %s".format(s)); None }
    s.split("\t") match {
      case Array(arg1Part, relPart, arg2Part, openparseConfString, enablerString, attrString, _*) => {
        try {
          val parts = Seq(arg1Part, relPart, arg2Part) map deserializePart
          val enabler = if (enablerString.equals("None")) None else Some(EnablingCondition.deserialize(enablerString))
          val attribution = if (attrString.equals("None")) None else Some(Attribution.deserialize(attrString))
          val extr = new OllieExtraction(parts(0), parts(1), parts(2), openparseConfString.toDouble, enabler, attribution)
          Some(extr)
        } catch {
          case e => { e.printStackTrace; error }
        }
      }
      case _ => error
    }
  }

  def serializePart(part: Part): String = {
    val serializedNodes = part.nodes.iterator.map(_.serialize).map(_.replaceAll("~", "_TILDE_")).mkString("~")
    Seq(part.text, serializedNodes).map(_.replaceAll("___", "_THREE_")).mkString("___")
  }

  def deserializePart(string: String): Part = {
    val Array(partText, partNodes) = try (string.split("___").map(_.replaceAll("_THREE_", "___")))
    catch {
      case e => throw new RuntimeException("could not deserialize Extraction.Part: " + string, e);
    }

    val nodesSortedSet: scala.collection.SortedSet[DependencyNode] = try (partNodes.split("~").map(_.replaceAll("_TILDE_", "~")).map(DependencyNode.deserialize(_))(breakOut))
    catch {
      case e => throw new RuntimeException("could not deserialize Extraction.Part: " + string, e);
    }
    new Part(nodesSortedSet, partText)
  }
}

