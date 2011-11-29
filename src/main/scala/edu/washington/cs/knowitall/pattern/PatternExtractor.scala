package edu.washington.cs.knowitall
package pattern

import scala.io.Source
import scopt.OptionParser
import tool.parse.graph._
import tool.parse.pattern._
import org.slf4j.LoggerFactory

class Extraction(
    val arg1: DependencyNode, 
    val rel: DependencyNode, 
    val arg2: DependencyNode) {
  override def toString() =
    Iterable(arg1, rel, arg2).mkString("(", ", ", ")")
}

object PatternExtractor {
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def toExtraction(graph: DependencyGraph, groups: collection.Map[String, DependencyNode]): Extraction = {
	def buildArgument(node: DependencyNode) = {
	  def cond(e: Graph.Edge[DependencyNode]) = 
	    e.label == "det" || e.label == "prep_of" || e.label == "amod" || e.label == "num" || e.label == "nn"
	  val inferiors = graph.graph.inferiors(node, cond)
	  val indices = inferiors.map(_.indices) reduce (_ ++ _)
	  // use the original dependencies nodes in case some information
	  // was lost.  For example, of is collapsed into the edge prep_of
	  val string = graph.nodes.filter(node => node.indices.max >= indices.min && node.indices.max <= indices.max).map(_.text).mkString(" ")
	  new DependencyNode(string, node.postag, node.indices)
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
  
  def validMatch(graph: Graph[DependencyNode])(m: Match[DependencyNode]) =
    !m.bipath.nodes.exists { v =>
      graph.edges(v).exists(_.label == "neg")
    }

  def score(extr: Extraction): Int = {
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
  
  def confidence(extr: Extraction, count: Int, maxCount: Int): Double = {
    count.toDouble / maxCount.toDouble
  }

  def extract(dgraph: DependencyGraph, pattern: Pattern[DependencyNode]) = {
    val matches = pattern(dgraph.graph).filter(validMatch(dgraph.graph))
    matches.map { m =>
      toExtraction(dgraph, m.groups)
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
      logger.info("Reading patterns...")
      val patternSource = Source.fromFile(parser.patternFilePath)
      val patterns = try {
        // parse the file
        patternSource.getLines.map { line =>
          line.split("\t") match {
            // full information specified
            case Array(pat, count) => (DependencyPattern.deserialize(pat), count.toInt)
            // assume a count of 1 if nothing is specified
            case Array(pat) => System.err.println("warning: pattern has no count: " + pat); (DependencyPattern.deserialize(pat), 1)
            case _ => throw new IllegalArgumentException("file can't have more than two columns")
          }
        // sort by inverse count so frequent patterns appear first 
        }.toList.sortBy(-_._2)
      } finally {
        patternSource.close
      }
      
      val totalCount = patterns.map(_._2).max

      logger.info("Performing extractions...")
      val sentenceSource = Source.fromFile(parser.sentenceFilePath)
      try {
        for (line <- sentenceSource.getLines) {
          val Array(text, deps) = line.split("\t")
          val nodes = text.split("\\s+").zipWithIndex.map{case (tok, i) => new DependencyNode(tok, null, i)}

          val dependencies = Dependencies.deserialize(deps)
          val dgraph = new DependencyGraph(text, nodes.toList, dependencies).collapseNounGroups.collapseNNPOf
          for ((pattern, count) <- patterns) {
            for (extr <- extract(dgraph, pattern)) {
              val conf = confidence(extr, count, totalCount)
              System.out.println(("%1.6f" format conf)+"\t"+extr+"\t"+pattern+"\t"+text+"\t"+deps)
            }
          }
        }
      } finally {
        sentenceSource.close
      }
    }
  }
}