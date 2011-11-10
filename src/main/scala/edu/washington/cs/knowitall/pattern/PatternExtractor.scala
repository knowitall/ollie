package edu.washington.cs.knowitall
package pattern

import scala.io.Source
import scopt.OptionParser
import tool.parse.graph._
import tool.parse.pattern._

object PatternExtractor {
  def toExtraction(groups: collection.Map[String, DependencyNode]): (DependencyNode, DependencyNode, DependencyNode) = {
    val rel = groups.find { case (s, dn) => s.equals("rel") }
    val arg1 = groups.find { case (s, dn) => s.equals("arg1") }
    val arg2 = groups.find { case (s, dn) => s.equals("arg2") }
    
    (rel, arg1, arg2) match {
      case (Some((_,rel)), Some((_,arg1)), Some((_,arg2))) => (arg1, rel, arg2)
      case _ => throw new IllegalArgumentException("missing group, expected {rel, arg1, arg2}: " + groups)
    }
  }

  def scoreExtraction(extr: (DependencyNode, DependencyNode, DependencyNode)): Int = {
    // helper methods
    def isProper(node: DependencyNode) = node.pos.equals("NNP") || node.pos.equals("NNPS")
    def isPrep(node: DependencyNode) = node.pos.equals("PRP") || node.pos.equals("PRPS")

    val arg1 = extr._1
    val arg2 = extr._3

    // pimped boolean
    class toInt(b: Boolean) {
      def toInt = if (b) 1 else 0
    }
    implicit def convertBooleanToInt(b: Boolean) = new toInt(b)

    2 + isProper(arg1).toInt + isProper(arg2).toInt + -isPrep(arg1).toInt + -isPrep(arg2).toInt
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
            val graph = new DependencyGraph(text, Dependencies.deserialize(deps)).collapseNounGroups.collapseNNPOf
            val matches = p(graph)
            val extractions = matches.map { m => 
              val extr = toExtraction(m.groups) 
              (scoreExtraction(extr), extr)
            }
            for ((score, extr) <- extractions) {
              System.out.println(score+"\t"+extr+"\t"+p+"\t"+text)
            }
          }
        }
      } finally {
        sentenceSource.close
      }
    }
  }
}
