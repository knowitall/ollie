package edu.washington.cs.knowitall
package pattern

import java.io.File
import java.io.PrintWriter
import scala.io.Source
import scala.collection
import common.Timing._
import TreePatternLearner.findPattern
import tool.parse._
import tool.stem._
import tool.parse.pattern._
import tool.parse.graph._
import util.DefaultObjects
import org.slf4j.LoggerFactory

object BuildTreePatterns {
  import TreePatternLearner._

  val logger = LoggerFactory.getLogger(this.getClass)

  val CHUNK_SIZE = 100000

  def main(args: Array[String]) {
    // file with dependencies
    val source = Source.fromFile(args(0))
    val writer = new PrintWriter(new File(args(1)))
    
    logger.info("chunk size: " + CHUNK_SIZE)

    var index = 0
    for (lines <- source.getLines.grouped(CHUNK_SIZE)) {
      @volatile var count = 0

      val ms = time(lines.par.foreach { line =>
        val Array(rel, arg1, arg2, lemmaString, text, _/*lemmas*/, _/*postags*/, _/*chunks*/, deps) = line.split("\t")
        val lemmas = lemmaString.split("\\s+").toSet

        // todo: push stemming forward in the process
        val dependencies = Dependencies.deserialize(deps).map(_.lemmatize(MorphaStemmer.instance))
        val graph = DependencyGraph(dependencies).normalize

        try {
          val patterns = findPatternsForLDA(graph, lemmas, Map(arg1 -> "arg1", arg2 -> "arg2"), rel, Some(2))
          for (pattern <- patterns) {
            val (pat, slots) = pattern
            if (slots.length == 0) {
              writer.println((List(rel, arg1, arg2, lemmas.mkString(" "), pat, text, deps) ::: slots).mkString("\t"))
              count += 1
            }
          }
        }
        catch {
          case e: NoRelationNodeException => // System.err.println(e); System.err.println(line)
        }
      })

      logger.info("chunk " + index + ": " + count + " items in " + Seconds.format(ms))
      writer.flush()

      index += 1
    }

    source.close
    writer.close
  }
}

object KeepCommonPatterns {
  def main(args: Array[String]) {
    val min = args(1).toInt

    val patterns = collection.mutable.HashMap[String, Int]().withDefaultValue(0)
    val firstSource = Source.fromFile(args(0))
    for (line <- firstSource.getLines) {
      val Array(rel, arg1, arg2, lemmas, pattern, text, deps, _*) = line.split("\t")
      patterns += pattern -> (patterns(pattern) + 1)
    }
    firstSource.close

    val secondSource = Source.fromFile(args(0))
    for (line <- secondSource.getLines) {
      val Array(rel, arg1, arg2, lemmas, pattern, text, deps, _*) = line.split("\t")
      if (patterns(pattern) >= min) {
        println(line)
      }
    }
    secondSource.close
  }
}