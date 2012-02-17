package edu.washington.cs.knowitall
package pattern
package extract

import scala.util.matching.Regex

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.parse.graph.Graph
import edu.washington.cs.knowitall.tool.parse.pattern.Pattern
import edu.washington.cs.knowitall.tool.parse.pattern.Match

import Template.group

class TemplateExtractor(val template: Template, pattern: Pattern[DependencyNode], patternCount: Int, maxPatternCount: Int) 
extends GeneralExtractor(pattern, patternCount, maxPatternCount) {
  override def extract(dgraph: DependencyGraph)(implicit
    buildExtraction: (DependencyGraph, Match[DependencyNode], PatternExtractor)=>Option[DetailedExtraction], 
    validMatch: Graph[DependencyNode]=>Match[DependencyNode]=>Boolean) = {

    val extractions = super.extractWithMatches(dgraph)

    extractions.map{ case (extr, m) => template(extr, m) }
  }
}

case class Template(template: String, be: Boolean) {
  import Template._
  def apply(extr: DetailedExtraction, m: Match[DependencyNode]) = {
    def matchGroup(name: String): String = name match {
      case "rel" => extr.rel
      case "arg1" => extr.arg1
      case "arg2" => extr.arg2
      case _ => m.groups(name).text
    }

    val prefix = if (be && ((extr.relNodes -- m.bipath.nodes) count (_.postag.startsWith("VB"))) == 0) {
      "be "
    }
    else ""

    // horrible escape is required.  See JavaDoc for Match.replaceAll
    // or https://issues.scala-lang.org/browse/SI-5437
    val rel = prefix + group.replaceAllIn(template, (gm: Regex.Match) => matchGroup(gm.group(1)).
      replaceAll("""\\""", """\\\\""").
      replaceAll("""\$""", """\\\$"""))

    extr.replaceRelation(rel)
  }
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
