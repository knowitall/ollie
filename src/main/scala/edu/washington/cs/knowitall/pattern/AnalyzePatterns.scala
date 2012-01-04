package edu.washington.cs.knowitall.pattern
import edu.washington.cs.knowitall.common.Resource
import scala.io.Source
import scala.collection.immutable.HashMap
import java.io.PrintWriter
import java.io.File
import scala.io.Codec

object AnalyzePatterns {
  def main(args: Array[String]) {
    val patternedFilePath = args(0)
    val outputFilePath = args(1)

    println("Counting pattern occurrence...")
    val patterns = collection.mutable.HashMap[String, Int]().withDefaultValue(0)
    Resource.using(Source.fromFile(patternedFilePath, "UTF8")) { source =>
      for (line <- source.getLines) {
        val Array(_, _, _, _, pattern, _, _, _*) = line.split("\t", -1)
        patterns += pattern -> (patterns(pattern) + 1)
      }
    }

    println("Grouping patterns...")
    Resource.using(new PrintWriter(new File(outputFilePath))) { writer =>
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