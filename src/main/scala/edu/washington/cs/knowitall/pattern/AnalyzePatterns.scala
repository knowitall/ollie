package edu.washington.cs.knowitall
package pattern

import tool.parse.pattern.DependencyPattern
import edu.washington.cs.knowitall.common.Resource
import scala.io.Source
import scala.collection.immutable
import scala.collection.mutable
import java.io.PrintWriter
import java.io.File
import scala.io.Codec
import tool.parse.pattern.DependencyEdgeMatcher
import tool.parse.pattern.LabelEdgeMatcher
import tool.parse.pattern.DirectedEdgeMatcher
import tool.parse.graph.DependencyNode
import tool.parse.graph.DependencyGraph
import tool.parse.pattern.PostagNodeMatcher

object AnalyzePatterns {
  def main(args: Array[String]) {
    val patternedFilePath = args(0)
    val outputFilePath = args(1)

    println("Counting pattern occurrence...")
    val patterns = mutable.HashMap[String, Int]().withDefaultValue(0)
    Resource.using(Source.fromFile(patternedFilePath, "UTF8")) { source =>
      for (line <- source.getLines) {
        val Array(_, _, _, _, pattern, _, _, _*) = line.split("\t", -1)
        patterns += pattern -> (patterns(pattern) + 1)
      }
    }

    println("Grouping patterns...")
    Resource.using(new PrintWriter(new File(outputFilePath), "UTF8")) { writer =>
      val ordered = patterns.toList.sortBy(_._2)(implicitly(Ordering[Int]).reverse)
      for ((pattern, count) <- ordered.filter(_._2 > 100)) {
        println(count + ":" + pattern)
        Resource.using(Source.fromFile(patternedFilePath, "UTF8")) { source =>
          writer.println(pattern + "\t" + count)
          for (line <- source.getLines) {
            val Array(rel, arg1, arg2, lemmas, p, sentence, deps, _*) = line.split("\t", -1)
            if (p == pattern) {
              writer.println(Iterable(rel, arg1, arg2, lemmas).mkString("\t"))
              writer.println(sentence)
              writer.println(deps)
              writer.println()
            }
          }
        }
      }

      println()
    }
  }
}

object CountPatternComponents {
  def main(args: Array[String]) {
    val patternedFilePath = args(0)

    val edgeCounts = mutable.HashMap[String, Int]().withDefaultValue(0)
    val postagCounts = mutable.HashMap[String, Int]().withDefaultValue(0)
    Resource.using(Source.fromFile(patternedFilePath, "UTF8")) { source =>
      for (line <- source.getLines) {
        val Array(_, _, _, _, pickledPattern, _, _, _*) = line.split("\t", -1)
        val pattern = new ExtractorPattern(DependencyPattern.deserialize(pickledPattern))
        val labels = (pattern.edgeMatchers.toList).flatMap { _ match {
              case e: DirectedEdgeMatcher[_] if e.matcher.isInstanceOf[LabelEdgeMatcher] => 
                Some(e.matcher.asInstanceOf[LabelEdgeMatcher].label)
              case _ => None
            }
        }
        val postags = (pattern.baseNodeMatchers.toList).collect {
          case m: PostagNodeMatcher => m.postag
        }
        
        for (l <- labels) {
          edgeCounts += l -> (edgeCounts(l)+1)
        }
        
        for (postag <- postags) {
          postagCounts += postag -> (postagCounts(postag)+1)
        }
      }
    }
    
    println("Postag counts: ")
    for ((k, v) <- postagCounts.toList.sortBy(_._2).reverse) {
      println(k + "\t" + v)
    }
    
    println()
    println("Edge counts: ")
    for ((k, v) <- edgeCounts.toList.sortBy(_._2).reverse) {
      println(k + "\t" + v)
    }
  }
}

object CountSentenceComponents {
  def main(args: Array[String]) {
    val patternedFilePath = args(0)

    val edgeCounts = mutable.HashMap[String, Int]().withDefaultValue(0)
    val postagCounts = mutable.HashMap[String, Int]().withDefaultValue(0)
    val pieceCounts = mutable.HashMap[String, Int]().withDefaultValue(0)
    Resource.using(Source.fromFile(patternedFilePath, "UTF8")) { source =>
      for (line <- source.getLines) {
        val Array(_, _, _, _, _, _, pickledGraph, _*) = line.split("\t", -1)
        val graph = DependencyGraph.deserialize(pickledGraph)
        val labels = (graph.graph.edges).toList.map(_.label )
        val postags = (graph.graph.vertices).toList.map(_.postag)
        
        for (l <- labels) {
          edgeCounts += l -> (edgeCounts(l)+1)
        }
        
        for (postag <- postags) {
          postagCounts += postag -> (postagCounts(postag)+1)
        }
        
        for (edge <- graph.graph.edges) {
          val piece1 = edge.source.postag + " " + edge.label + " " + edge.dest.postag
          val piece2 = edge.dest.postag + " " + edge.label + " " + edge.source.postag
          
          pieceCounts += piece1 -> (pieceCounts(piece1)+1)
          pieceCounts += piece2 -> (pieceCounts(piece2)+1)
        }
      }
    }
    
    println("Postag counts: ")
    for ((k, v) <- postagCounts.toList.sortBy(_._2).reverse) {
      println(k + "\t" + v)
    }
    
    println()
    println("Edge counts: ")
    for ((k, v) <- edgeCounts.toList.sortBy(_._2).reverse) {
      println(k + "\t" + v)
    }
    
    println()
    println("Piece counts: ")
    for ((k, v) <- pieceCounts.toList.sortBy(_._2).reverse) {
      println(k + "\t" + v)
    }
  }
}