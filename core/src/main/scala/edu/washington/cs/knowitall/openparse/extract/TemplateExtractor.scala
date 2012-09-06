package edu.washington.cs.knowitall.openparse.extract

import scala.util.matching.Regex
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.collection.immutable.graph.pattern.{Pattern, Match}
import edu.washington.cs.knowitall.collection.immutable.graph.Graph
import edu.washington.cs.knowitall.tool.parse.graph.{DependencyPattern, DependencyNode, DependencyGraph}
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer.instance
import Template.group
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer

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
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Iterable[DetailedExtraction],
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {

    val extractions = super.extractWithMatches(dgraph)

    extractions.map{ case (extr, m) => template(extr, dgraph, m) }
  }

  override def tabSerialize = Iterable("Template", template.serialize, pattern.serialize, conf.toString).mkString("\t")
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

  def tabDeserialize(string: String) = {
    val parts = string.split("\t")
  }

  def tabDeserialize(parts: Seq[String]): (TemplateExtractor, Seq[String]) = {
    val Seq(templateString, patternString, confString, rest @ _*) = parts

    val template = Template.deserialize(templateString)
    val pattern = DependencyPattern.deserialize(patternString)
    val conf = confString.toDouble

    (new TemplateExtractor(template, pattern, conf), rest)
  }
}

case class Template(template: String, be: Boolean) {
  import Template._
  def apply(extr: DetailedExtraction, dgraph: DependencyGraph, m: Match[DependencyNode]) = {
    def matchGroup(name: String): String = name match {
      case "rel" => extr.relText
      case "arg1" => extr.arg1Text
      case "arg2" => extr.arg2Text
      case _ => m.groups(name).text
    }

    // don't add the be if we attach a verb using a cop, aux, or auxpass edge.
    // there are a lot of examples where adding "be" makes it very messy
    //     "She has practiced law, with Foo, Bar."
    //     don't want: (Bar; be has practiced with; Foo)
    // This is somewhat of a hack that makes bad patterns look less bad.
    val prefix = if (be &&
        !(dgraph.graph.neighbors(m.nodeGroups("rel").node, dedge => (dedge.edge.label startsWith "aux") || dedge.edge.label == "cop") filter (_.postag startsWith "VB") exists (neighbor => extr.rel.nodes contains neighbor))) {
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

  def serialize = this.toString
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
