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
        case Some(file) => new PrintWriter(file, "UTF8")
        case None => new PrintWriter(System.out)
      }
    } { writer =>
      for ((conf, yld, pr) <- Analysis.precisionYieldMeta(input)) {
        writer.println(conf + "\t" + yld + "\t" + pr)
      }
    }
  }
}

object MergePYFiles {
  abstract class Settings {
    def files: List[File]
  }
  
  def main(args: Array[String]) {
    val settings = new Settings {
      var files: List[File] = Nil
    }
    
    val parser = new OptionParser("mergebycol") {
      arglist("<file>...", "input files", { file: String => settings.files = new File(file) :: settings.files })
    }
    
    if (parser.parse(args)) {
      run(settings)
    }
  }

  def run(settings: Settings) {
    val points = for ((file, i) <- settings.files.zipWithIndex) yield {
      using(io.Source.fromFile(file, "UTF8")) { source =>
        source.getLines.map { line =>
          val Array(_, yld, prec) = line.split("\t", -1)
          (yld.toInt, (i, prec.toDouble))
        }.toList
      }
    }
    
    println("\t" + settings.files.map(_.getName).mkString("\t"))
    points.flatten.sortBy(_._1).reverse foreach { case (k, (i, v)) => println(k+"\t"+"\t"*i+v) }
  }
}