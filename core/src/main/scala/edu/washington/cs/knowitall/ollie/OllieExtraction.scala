package edu.washington.cs.knowitall.ollie

import scala.Array.canBuildFrom
import scala.collection.breakOut

import OllieExtraction.serializePart
import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.openparse.extract.Extraction.Part
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode

abstract class Clausal {
  def text: String
  def interval: Interval
}

class EnablingCondition(val prefix: String, val phrase: String, override val interval: Interval) extends Clausal {
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

class Attribution(val arg: String, val argInterval: Interval, val rel: String, override val interval: Interval) extends Clausal {
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

class OllieExtraction(
  val arg1: Part,
  val rel: Part,
  val arg2: Part,
  val confidence: Double,
  val enabler: Option[EnablingCondition],
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
    
    val fieldStrings = Seq(arg1, rel, arg2).map(serializePart(_)) ++ Seq("%.05f".format(confidence), enablerString, attrString)
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

object OllieExtraction {
  
  def tabDelimitedColumns = Seq("Arg1Part", "RelPart", "Arg2Part", "Confidence", "Enabler", "Attribution").mkString("\t")

  def deserialize(s: String): Option[OllieExtraction] = {
    def error = { System.err.println("Couldn't deserialize: %s".format(s)); None }
    s.split("\t") match {
      case Array(arg1Part, relPart, arg2Part, confString, enablerString, attrString, _*) => {
        try {
          val parts = Seq(arg1Part, relPart, arg2Part) map deserializePart
          val enabler = if (enablerString.equals("None")) None else Some(EnablingCondition.deserialize(enablerString))
          val attribution = if (attrString.equals("None")) None else Some(Attribution.deserialize(attrString))
          val extr = new OllieExtraction(parts(0), parts(1), parts(2), confString.toDouble, enabler, attribution)
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

