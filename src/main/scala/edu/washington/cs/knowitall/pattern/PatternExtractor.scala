package edu.washington.cs.knowitall
package pattern

import scala.io.Source
import scopt.OptionParser
import tool.parse.graph._
import tool.parse.pattern._

class Extraction(
    val arg1: DependencyNode, 
    val rel: DependencyNode, 
    val arg2: DependencyNode) {
  override def toString() =
    Iterable(arg1, rel, arg2).mkString("(", ", ", ")")
}

object PatternExtractor {
  def toExtraction(graph: DependencyGraph, groups: collection.Map[String, DependencyNode]): Extraction = {
	def buildArgument(node: DependencyNode) = {
	  def cond(e: Edge[DependencyNode]) = 
	    e.label == "det" || e.label == "prep_of" || e.label == "amod" || e.label == "CD"
	  val inferiors = graph.graph.inferiors(node, cond).map(_.index)
	  // use the original dependencies nodes in case some information
	  // was lost.  For example, of is collapsed into the edge prep_of
	  val string = graph.nodes.get.slice(inferiors.min, inferiors.max+1).map(_.text).mkString(" ")
	  new DependencyNode(string, node.postag, node.index)
	}
	
    val rel = groups.find { case (s, dn) => s.equals("rel") }
    val arg1 = groups.find { case (s, dn) => s.equals("arg1") }
    val arg2 = groups.find { case (s, dn) => s.equals("arg2") }
    
    (rel, arg1, arg2) match {
      case (Some((_,rel)), Some((_,arg1)), Some((_,arg2))) => 
        new Extraction(buildArgument(arg1), rel, buildArgument(arg2))
      case _ => throw new IllegalArgumentException("missing group, expected {rel, arg1, arg2}: " + groups)
    }
  }

  def scoreExtraction(extr: Extraction): Int = {
    // helper methods
    def isProper(node: DependencyNode) = node.postag.equals("NNP") || node.postag.equals("NNPS")
    def isPrep(node: DependencyNode) = node.postag.equals("PRP") || node.postag.equals("PRPS")

    // pimped boolean
    class toInt(b: Boolean) {
      def toInt = if (b) 1 else 0
    }
    implicit def convertBooleanToInt(b: Boolean) = new toInt(b)

    2 + isProper(extr.arg1).toInt + isProper(extr.arg2).toInt + -isPrep(extr.arg1).toInt + -isPrep(extr.arg2).toInt
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
        patternSource.getLines.map(DependencyPattern.deserialize(_)).toList
      } finally {
        patternSource.close
      }

      val sentenceSource = Source.fromFile(parser.sentenceFilePath)
      try {
        for (line <- sentenceSource.getLines) {
          val Array(text, deps) = line.split("\t")
          for (p <- patterns) {
            val dependencies = Dependencies.deserialize(deps)
            val nodes = text.split("\\s+").zipWithIndex.map{case (tok, i) => new DependencyNode(tok, null, i)}
            val dgraph = new DependencyGraph(text, nodes.toArray, dependencies).collapseNounGroups.collapseNNPOf
            val matches = p(dgraph.graph)
            val extractions = matches.map { m => 
              val extr = toExtraction(dgraph, m.groups) 
              (scoreExtraction(extr), extr)
            }
            for ((score, extr) <- extractions) {
              System.out.println(score+"\t"+extr+"\t"+p+"\t"+text+"\t"+deps)
            }
          }
        }
      } finally {
        sentenceSource.close
      }
    }
  }
}
