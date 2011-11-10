package edu.washington.cs.knowitall
package pattern

import scala.io.Source
import scopt.OptionParser
import tool.parse.graph._
import tool.parse.pattern._

object PatternExtractor {
  def toExtraction(groups: collection.Map[String, DependencyNode]) = {
    val rel = groups.find { case (s, dn) => s.equals("rel") }
    val arg1 = groups.find { case (s, dn) => s.equals("arg1") }
    val arg2 = groups.find { case (s, dn) => s.equals("arg2") }
    
    (rel, arg1, arg2) match {
      case (Some((_,rel)), Some((_,arg1)), Some((_,arg2))) => (arg1, rel, arg2)
      case _ => throw new IllegalArgumentException("missing group, expected {rel, arg1, arg2}: " + groups)
    }
  }
  
  def main(args: Array[String]) {
    val parser = new OptionParser("applypat") {
      var patternFilePath: String = null
      var sentenceFilePath: String = null
      opt("p", "patterns", "<file>", "pattern file", { v: String => patternFilePath = v })
      opt("s", "sentences", "<file>", "sentence file", { v: String => sentenceFilePath = v })
    }

    if (parser.parse(args)) {
      val patternSource = Source.fromFile(parser.patternFilePath)
      val patterns = try {
        patternSource.getLines.map(Pattern.deserialize(_)).toList
      } finally {
        patternSource.close
      }

      val sentenceSource = Source.fromFile(parser.sentenceFilePath)
      try {
        for (line <- sentenceSource.getLines) {
          val Array(text, deps) = line.split("\t")
          for (p <- patterns) {
            val graph = new DependencyGraph(text, Dependencies.deserialize(deps))
            for (m <- p(graph)) {
              System.out.println(toExtraction(m.groups)+"\t"+p+"\t"+text)
            }
          }
        }
      } finally {
        sentenceSource.close
      }
    }
  }
}