package edu.washington.cs.knowitall
package pattern.eval

import edu.washington.cs.knowitall.common.Resource._
import scopt.OptionParser
import scala.io.Source
import java.io.File
import java.io.PrintWriter

object Score {
  def main(args: Array[String]) = {
    val parser = new OptionParser("scoreextr") {
      var extractionFile: File = _
      var outputFile: File = _
      var goldFile: Option[File] = None
      var goldOutputFile: Option[File] = None

      arg("extrs", "extractions", { path: String => extractionFile = new File(path) })
      arg("output", "scored output", { path: String => outputFile = new File(path) })
      opt("g", "gold", "gold set", { path: String => goldFile = Some(new File(path)) })
      opt("u", "goldoutput", "output for updated gold set", { path: String => goldOutputFile = Some(new File(path)) })
    }

    if (parser.parse(args)) {
      val gold = parser.goldFile match {
        case None => Map[String, Boolean]()
        case Some(goldFile) => loadGoldSet(goldFile)
      }

      val (scoreds, golden) = using(Source.fromFile(parser.extractionFile)) { source =>
        score(source.getLines, gold)
      }
      
      // print the scored extractions
      using (new PrintWriter(parser.outputFile)) { writer =>
        for (scored <- scoreds) {
          writer.println(scored.toRow)
        }
      }

      // output updated gold set
      parser.goldOutputFile match {
        case Some(file) => 
          using(new PrintWriter(file)) { writer =>
            golden.foreach { case (k, v) => writer.println((if (v) 1 else 0) + "\t" + k) }
          }
        case None =>
      }
    }
  }
  
  def loadScoredFile(file: File) = {
    using(Source.fromFile(file)) { source =>
      source.getLines.map { line =>
        Scored.fromRow(line)
      }.toList
    }
  }

  def loadGoldSet(file: File) = {
    using(Source.fromFile(file)) { source =>
      source.getLines.map { line =>
        val parts = line.split("\t")
        parts(1) -> (if (parts(0) == "1") true else false)
      }.toMap
    }
  }

  def score(lines: Iterator[String], gold: Map[String, Boolean]) = {
    def promptScore(extr: String, confidence: String, rest: Seq[Any]): Option[Boolean] = {
      println()
      System.out.println("Please score " + confidence + ":" + extr + ". (1/0) ")
      if (rest.length > 0) println(rest.mkString("\t"))
      readLine match {
        case "0" => Some(false)
        case "1" => Some(true)
        case "skip" => None
        case _ => promptScore(extr, confidence, rest)
      }
    }

    var golden = gold

    val scored = for {
      line <- lines
      val Array(confidence, extr, rest @ _*) = line.split("\t")

      val scoreOption = gold.get(extr) match {
        case Some(score) => Some(score)
        case None => promptScore(extr, confidence, rest)
      }
      
      if scoreOption.isDefined
      val score = scoreOption.get
    } yield {
      // update golden set
      golden += extr -> score
      // output
      Scored(score, confidence.toDouble, extr, rest)
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