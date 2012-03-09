package edu.washington.cs.knowitall.pattern.eval

import edu.washington.cs.knowitall.common.Resource._
import scopt.OptionParser
import java.io.File
import scala.io.Source
import common.stats.Analysis
import java.io.PrintWriter

object PrecisionYield {
  abstract class Settings {
    def scoredFile: File
    def outputFile: Option[File]
  }
  
  def main(args: Array[String]) = {
    val settings = new Settings {
      var scoredFile: File = _
      var outputFile: Option[File] = None 
    }
    
    val parser = new OptionParser("precyield") {
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
    val input = scores.map(scored => ("%.4f".format(scored.confidence), scored.score))

    using {
      settings.outputFile match {
        case Some(file) => new PrintWriter(file)
        case None => new PrintWriter(System.out)
      }
    } { writer =>
      for ((conf, yld, pr) <- Analysis.precisionYieldMeta(input)) {
        writer.println(conf + "\t" + yld + "\t" + pr)
      }
    }
  }
}