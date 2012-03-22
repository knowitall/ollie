package edu.washington.cs.knowitall
package pattern.eval

import edu.washington.cs.knowitall.common.Resource._
import scopt.OptionParser
import scala.io.Source
import java.io.File
import java.io.PrintWriter

object Score {
  abstract class Settings {
    def extractionFile: File
    def outputFile: File
    def goldFile: Option[File]
    def goldOutputFile: Option[File]
    def confidenceThreshold: Double
    def skipAll: Boolean
  }
  
  def main(args: Array[String]) = {
    object settings extends Settings {
      var extractionFile: File = _
      var outputFile: File = _
      var goldFile: Option[File] = None
      var goldOutputFile: Option[File] = None
      var confidenceThreshold = 0.0
      var skipAll = false
    }
    
    val parser = new OptionParser("scoreextr") {
      arg("extrs", "extractions", { path: String => settings.extractionFile = new File(path) })
      arg("output", "scored output", { path: String => settings.outputFile = new File(path) })
      opt("g", "gold", "gold set", { path: String => settings.goldFile = Some(new File(path)) })
      opt("u", "goldoutput", "output for updated gold set", { path: String => settings.goldOutputFile = Some(new File(path)) })
      doubleOpt("t", "threshold", "confidence threshold for considered extractions", { x: Double => settings.confidenceThreshold = x })
      opt("skip-all", "don't prompt for items not in the gold set", { settings.skipAll = true })
    }

    if (parser.parse(args)) {
    }
  }

  def run(settings: Settings) {
    val gold = settings.goldFile match {
      case None => Map[String, Boolean]()
      case Some(goldFile) => loadGoldSet(goldFile)
    }

    val (scoreds, golden) = using(Source.fromFile(settings.extractionFile, "UTF8")) { source =>
      score(source.getLines, gold, settings.confidenceThreshold, !settings.skipAll)
    }

    // print the scored extractions
    using(new PrintWriter(settings.outputFile, "UTF8")) { writer =>
      for (scored <- scoreds) {
        writer.println(scored.toRow)
      }
    }

    // output updated gold set
    settings.goldOutputFile match {
      case Some(file) =>
        using(new PrintWriter(file, "UTF8")) { writer =>
          golden.foreach { case (k, v) => writer.println((if (v) 1 else 0) + "\t" + k) }
        }
      case None =>
    }
  }
  
  def loadScoredFile(file: File) = {
    using(Source.fromFile(file, "UTF8")) { source =>
      source.getLines.map { line =>
        Scored.fromRow(line)
      }.toList
    }
  }

  def loadGoldSet(file: File) = {
    using(Source.fromFile(file, "UTF8")) { source =>
      source.getLines.map { line =>
        val parts = line.split("\t")
        parts(1) -> (if (parts(0) == "1") true else false)
      }.toMap
    }
  }

  def score(lines: Iterator[String], gold: Map[String, Boolean], confidenceThreshold: Double, prompt: Boolean) = {
    def promptScore(index: Int, extr: String, confidence: String, rest: Seq[Any]): Option[Boolean] = {
      println()
      System.out.println("Please score " + index + ": " + confidence + ":" + extr + ". (1/y/0/n/skip) ")
      if (rest.length > 0) println(rest.mkString("\t"))
      readLine match {
        case "0" | "y" => Some(false)
        case "1" | "n" => Some(true)
        case "s" | "skip" => None
        case _ => promptScore(index, extr, confidence, rest)
      }
    }
    
    var golden = gold
    
    val scored = for {
      (line, index) <- lines.zipWithIndex
      val Array(confidence, extr, rest @ _*) = line.split("\t")
      val conf = confidence.toDouble
      
      if (conf >= confidenceThreshold)
      
      val scoreOption = gold.get(extr) match {
        case Some(score) => Some(score)
        case None if prompt => promptScore(index, extr, confidence, rest)
        case None => None
      }
      
      if scoreOption.isDefined
      val score = scoreOption.get
    } yield {
      // update golden set
      golden += extr -> score
      // output
      Scored(score, conf, extr, rest)
    }

    (scored.toList, golden)
  }
}

case class Scored(score: Boolean, confidence: Double, extraction: String, extra: Seq[String]) {
  def toRow = (if (score) "1" else "0")+"\t"+confidence+"\t"+extraction+"\t"+extra.mkString("\t")
}

object Scored {
  def fromRow(row: String) = {
    val parts = row.split("\t")
    val score = parts(0) match {
      case "1" => true
      case "0" => false
      case _ => throw new IllegalArgumentException("must be 1 or 0: " + parts(0))
    }
    val confidence = parts(1).toDouble
    val extraction = parts(2)
    val extra = parts.drop(3)
    
    Scored(score, confidence, extraction, extra)
  }
}