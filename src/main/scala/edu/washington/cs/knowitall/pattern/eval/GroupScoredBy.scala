package edu.washington.cs.knowitall.pattern.eval

import java.io.File

import edu.washington.cs.knowitall.common.Analysis

import scopt.OptionParser

object GroupScoredBy {
  def main(args: Array[String]) = {
    val parser = new OptionParser("precyield") {
      var scoredFile: File = _
      var column: Int = 2

      arg("scored", "scored extractions", { path: String => scoredFile = new File(path) })
      intOpt("k", "column", "column", { c: Int => column = c })
    }

    if (parser.parse(args)) {
      require(parser.column >= 2, "column must be >= 2")

      val scores = Score.loadScoredFile(parser.scoredFile)
      val grouped = scores.groupBy(scored => scored.extra(parser.column - 2))

      val scored = (for (group <- grouped) yield {
        val title = group._1
        val scoreds = group._2

        (group._1, Analysis.precision(scoreds.map(scored => scored.score.getOrElse(throw new IllegalArgumentException("unscored extraction: " + scored)))), group._2)
      }).toList.sortBy(tuple => (tuple._2, tuple._3.mkString("\t"))).reverse
      
      scored.foreach { item => 
        println(item._2 + ": " + item._1)
        item._3.sortBy(scored => (scored.confidence, scored.toRow)).iterator.map(_.toRow).foreach(println)
        println()
      }
    }
  }
}
