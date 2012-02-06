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

import scopt.OptionParser

object BuildTreePatterns {
  import TreePatternLearner._

  val logger = LoggerFactory.getLogger(this.getClass)

  val CHUNK_SIZE = 100000

  class Settings {
    var sourcePath: String = _
    var destPath: Option[String] = None
    var length = Option.empty[Int]
    var parallel = false
  }

  def main(args: Array[String]) {
    val settings = new Settings

    val parser = new OptionParser("buildpats") {
      arg("source", "source", { v: String => settings.sourcePath = v })
      argOpt("dest", "dest", { v: String => settings.destPath = Some(v) })
      opt("p", "parallel", "run multithreaded", { settings.parallel = true })
      intOpt("l", "length", "<length>", "maximum number of edges in the patterns", { l: Int => settings.length = Some(l) })
    }
    if (parser.parse(args)) {
      main(settings)
    }
  }
 
 def main(settings: Settings) {
   def validGraph(graph: DependencyGraph) = {
     // make sure there is a verb
     graph.nodes.exists(node => "(?i)^VB".r.findFirstIn(node.text).isDefined)
   }
   
    // file with dependencies
    val source = Source.fromFile(settings.sourcePath)
    val writer = settings.destPath.map(dest => new PrintWriter(new File(dest))).getOrElse(new PrintWriter(System.out))
    
    logger.info("chunk size: " + CHUNK_SIZE)
    logger.info("pattern length: " + settings.length)

    var index = 0
    for (lines <- source.getLines.grouped(CHUNK_SIZE)) {
      @volatile var count = 0

      val group = if (settings.parallel) lines.par else lines

      val ms = time(group.foreach { line =>
        val Array(rel, arg1, arg2, lemmaString, text, _/*lemmas*/, _/*postags*/, _/*chunks*/, deps) = line.split("\t")
        val lemmas = lemmaString.split("\\s+").toSet

        // todo: push stemming forward in the process
        try {
          val graph = DependencyGraph.deserialize(deps).map { node =>
            node.lemmatize(MorphaStemmer.instance)
          }.normalize

          if (!validGraph(graph)) {
            logger.warn("Invalid graph (no verb?): " + graph.text + "\t" + graph.serialize)
          }
          else {
            val patterns = findPatternsForLDA(graph, lemmas, Map(arg1 -> "arg1", arg2 -> "arg2"), rel, settings.length)
            for ((pattern, slots) <- patterns; if pattern.valid) {
              if (!settings.length.isDefined || pattern.nodeMatchers.length <= settings.length.get) {
                writer.println((List(rel, arg1, arg2, lemmas.mkString(" "), pattern, text, deps) ::: slots).mkString("\t"))
                count += 1
              }
            }
          }
        }
        catch {
          case e: NoRelationNodeException => logger.warn(e.toString)
          case e: DependencyGraph.SerializationException => 
            logger.error("could not deserialize graph: " + deps, e)
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
    System.err.println("minimum pattern size: "+min)

    var rows = 0
    var keepers = 0

    val patterns = collection.mutable.HashMap[String, Int]().withDefaultValue(0)
    val firstSource = Source.fromFile(args(0))
    for (line <- firstSource.getLines) {
      val Array(rel, arg1, arg2, lemmas, pattern, text, deps, _*) = line.split("\t")
      rows += 1
      patterns += pattern -> (patterns(pattern) + 1)
    }
    firstSource.close

    System.err.println(rows+" rows")
    System.err.println(patterns.size+" unique patterns")

    val secondSource = Source.fromFile(args(0))
    for (line <- secondSource.getLines) {
      val Array(rel, arg1, arg2, lemmas, pattern, text, deps, _*) = line.split("\t")
      if (patterns(pattern) >= min) {
        keepers += 1
        println(line)
      }
    }
    secondSource.close

    System.err.println(keepers+" patterns that occur more than "+min+"times") 
  }
}
