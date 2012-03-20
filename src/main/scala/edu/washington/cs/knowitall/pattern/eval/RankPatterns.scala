package edu.washington.cs.knowitall.pattern.eval

import edu.washington.cs.knowitall.common.Resource._
import scopt.OptionParser
import java.io.File
import scala.io.Source
import common.stats.Analysis
import java.io.PrintWriter

object RankPatterns {
  abstract class Settings {
    def scoredFile: File
    def outputFile: Option[File]
  }
  
  def main(args: Array[String]) = {
    val settings = new Settings {
      var scoredFile: File = _
      var outputFile: Option[File] = None 
    }
    
    val parser = new OptionParser("rankpat") {
      var scoredFile: File = _

      arg("scored", "scored extractions file", { path: String => settings.scoredFile = new File(path) })
      argOpt("output", "output file", { path: String => settings.outputFile = Some(new File(path)) })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }
  
  def run(settings: Settings) = {
    val scores = Score.loadScoredFile(settings.scoredFile).sortBy(_.confidence).reverse
    val grouped = scores.groupBy(_.extra(0))
      .mapValues(scoreds => scoreds.map(x => if (x.score) 1 else 0).sum.toDouble / scoreds.size.toDouble)

    using {
      settings.outputFile match {
        case Some(file) => new PrintWriter(file)
        case None => new PrintWriter(System.out)
      }
    } { writer =>
      for ((pattern, score) <- grouped.toSeq.sortBy(-_._2)) {
        println(pattern+"\t"+score)
      }
    }
  }
}
