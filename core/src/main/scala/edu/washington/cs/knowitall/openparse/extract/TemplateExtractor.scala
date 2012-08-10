package edu.washington.cs.knowitall.openparse.extract

import scala.util.matching.Regex

import org.slf4j.LoggerFactory

import edu.washington.cs.knowitall.collection.immutable.graph.pattern.{Pattern, Match}
import edu.washington.cs.knowitall.collection.immutable.graph.Graph
import edu.washington.cs.knowitall.tool.parse.graph.{DependencyPattern, DependencyNode, DependencyGraph}
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer.instance

import Template.group

/** An extractor that is specified by a pattern and a template.
  * the template can add a "to be" and/or preposition word around
  * the relation.  It can also change the preposition word to another
  * preposition (i.e., switch "of" to "in").
  * 
  * @author Michael Schmitz
  */
class TemplateExtractor(val template: Template, pattern: Pattern[DependencyNode], conf: Double)
extends GeneralExtractor(pattern, conf) {
  override def extract(dgraph: DependencyGraph)(implicit
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Option[DetailedExtraction],
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {

    val extractions = super.extractWithMatches(dgraph)

    extractions.map{ case (extr, m) => template(extr, m) }
  }
}

case object TemplateExtractor extends PatternExtractorType {
  val logger = LoggerFactory.getLogger(this.getClass)

  override def fromLines(lines: Iterator[String]): List[PatternExtractor] = {
    val patterns: List[(Template, Pattern[DependencyNode], Double)] = lines.map { line =>
      line.split("\t") match {
        // full information specified
        case Array(template, pat, conf) =>
          (Template.deserialize(template), DependencyPattern.deserialize(pat), conf.toDouble)
        // assume a count of 1 if nothing is specified
        case Array(template, pat) =>
          logger.warn("warning: pattern has no confidence: " + pat);
          (Template.deserialize(template), DependencyPattern.deserialize(pat), 1.0)
        case _ => throw new IllegalArgumentException("line must have two or three columns: " +line)
      }
    }.toList

    val maxCount = patterns.maxBy(_._3)._3
    (for ((template, pattern, conf) <- patterns) yield {
      new TemplateExtractor(template, pattern, conf)
    }).toList
  }
}

case class Template(template: String, be: Boolean) {
  import Template._
  def apply(extr: DetailedExtraction, m: Match[DependencyNode]) = {
    def matchGroup(name: String): String = name match {
      case "rel" => extr.relText
      case "arg1" => extr.arg1Text
      case "arg2" => extr.arg2Text
      case _ => m.groups(name).text
    }

    val prefix = if (be && ((extr.rel.nodes -- m.bipath.nodes) count (_.postag.startsWith("VB"))) == 0) {
      "be"
    }
    else ""

    // pull out the modals because they must preceed the prefix
    // also include "to"
    val modals = extr.rel.nodes.filter(node => (node.postag startsWith "MD") || 
        (node.postag == "TO"))

    // horrible escape is required.  See JavaDoc for Match.replaceAll
    // or https://issues.scala-lang.org/browse/SI-5437
    var rel = group.replaceAllIn(template, (gm: Regex.Match) => matchGroup(gm.group(1))
      .replaceAll("_", " ")
      .replaceAll("""\\""", """\\\\""")
      .replaceAll("""\$""", """\\\$"""))

    if (!prefix.isEmpty) {
      if (modals.isEmpty) {
        rel = prefix + " " + rel
      } else {
        val regex = new Regex("(^.*\\b(?:" + modals.iterator.map(_.text).mkString("|") + "))\\b")
        rel = regex.replaceAllIn(rel, "$1 " + prefix)
      }
    }

    extr.replaceRelation(rel)
  }

  override def toString = (if (be) "be " else "") + template
}

object Template {
  val group = """\{(.*?)}""".r
  def deserialize(string: String) = {
    if (string.startsWith("be ")) {
      Template(string.drop(3), true)
    }
    else {
      Template(string, false)
    }
  }
}
