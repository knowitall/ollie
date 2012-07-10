package edu.washington.cs.knowitall.openparse.eval

import java.io.{PrintWriter, File}

import edu.washington.cs.knowitall.common.Resource.using

import scopt.OptionParser

/** Read a scored file and rank the patterns by their precision and frequency.
  * 
  * @author Michael Schmitz
  */
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
      .mapValues { scoreds =>
        val yld = scoreds.map(scored => if (scored.score.getOrElse(throw new IllegalArgumentException("unscored extraction: " + scored))) 1 else 0).sum
        val precision = yld.toDouble / scoreds.size.toDouble
        (precision, scoreds.size)
      }

    using {
      settings.outputFile match {
        case Some(file) => new PrintWriter(file, "UTF8")
        case None => new PrintWriter(System.out)
      }
    } { writer =>
      for ((pattern, (p, y)) <- grouped.toSeq.sortBy(_._2).reverse) {
        writer.println(pattern+"\t"+p+"\t"+y)
      }
    }
  }
}
