package edu.washington.cs.knowitall.pattern.eval

import java.io.{PrintWriter, File}

import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.common.Analysis

import scopt.OptionParser

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
    val input = scores.map(scored => ("%.4f".format(scored.confidence), scored.score.getOrElse(throw new IllegalArgumentException("unscored extraction: " + scored))))

    using {
      settings.outputFile match {
        case Some(file) => new PrintWriter(file, "UTF8")
        case None => new PrintWriter(System.out)
      }
    } { writer =>
      val py = Analysis.precisionYieldMeta(input)
      val area = Analysis.areaUnderCurve(py.map { case (conf, yld, pr) => (yld, pr) })
      println("auc: " + area)
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
    points.flatten.sortBy(_._1).reverse.groupBy(_._1).toSeq.sortBy(_._1).reverse foreach { case (grp, seq) => 
      var vec = Vector.fill[String](settings.files.size)("")
      seq.foreach { 
        case (k, (i, v)) => vec = vec updated (i, v.toString)
      }
      println(grp+"\t"+vec.mkString("\t")) 
    }
  }
}
