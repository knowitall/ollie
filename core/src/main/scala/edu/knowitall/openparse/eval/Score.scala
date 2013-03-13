package edu.knowitall.openparse.eval

import java.io.{PrintWriter, File}

import scala.io.Source

import edu.knowitall.common.Resource.using

import scopt.OptionParser

/** A main method to annotate extractions,
  * using a gold set for previously scored extractions.
  *
  * @author Michael Schmitz
  */
object Score {
  abstract class Settings {
    def extractionFile: File
    def outputFile: File
    def goldFile: Option[File]
    def goldOutputFile: Option[File]
    def confidenceThreshold: Double
    def skipAll: Boolean
    def keepSkipped: Boolean
  }

  def main(args: Array[String]) = {
    object settings extends Settings {
      var extractionFile: File = _
      var outputFile: File = _
      var goldFile: Option[File] = None
      var goldOutputFile: Option[File] = None
      var confidenceThreshold = 0.0
      var skipAll = false
      var keepSkipped = false
    }

    val parser = new OptionParser("scoreextr") {
      arg("extrs", "extractions", { path: String => settings.extractionFile = new File(path) })
      arg("output", "scored output", { path: String => settings.outputFile = new File(path) })
      opt("g", "gold", "gold set", { path: String => settings.goldFile = Some(new File(path)) })
      opt("u", "goldoutput", "output for updated gold set", { path: String => settings.goldOutputFile = Some(new File(path)) })
      doubleOpt("t", "threshold", "confidence threshold for considered extractions", { x: Double => settings.confidenceThreshold = x })
      opt("skip-all", "don't prompt for items not in the gold set", { settings.skipAll = true })
      opt("keep-skipped", "keep unannotated extractions in output file", { settings.keepSkipped = true })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }

  def run(settings: Settings) {
    val gold = settings.goldFile match {
      case None => Map[String, Boolean]()
      case Some(goldFile) => GoldSet.load(goldFile)
    }

    val (scoreds, golden) = using(Source.fromFile(settings.extractionFile, "UTF8")) { source =>
      score(source.getLines, gold, settings.confidenceThreshold, !settings.skipAll)
    }

    // print the scored extractions
    using(new PrintWriter(settings.outputFile, "UTF8")) { writer =>
      for (scored <- scoreds.filter(scored => settings.keepSkipped || scored.score.isDefined)) {
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

  def loadScoredFile(file: File): Seq[Scored] = {
    using(Source.fromFile(file, "UTF8")) { source =>
      source.getLines.map { line =>
        Scored.fromRow(line)
      }.toList
    }
  }

  def score(lines: Iterator[String], gold: Map[String, Boolean], confidenceThreshold: Double, prompt: Boolean) = {
    def stringDistance(s1: String, s2: String): Int = {
      def minimum(i1: Int, i2: Int, i3: Int) = math.min(math.min(i1, i2), i3)

      val dist = Array.ofDim[Int](s1.length + 1, s2.length + 1)

      for (idx <- 0 to s1.length) dist(idx)(0) = idx
      for (jdx <- 0 to s2.length) dist(0)(jdx) = jdx

      for (idx <- 1 to s1.length; jdx <- 1 to s2.length)
        dist(idx)(jdx) = minimum (
          dist(idx-1)(jdx  ) + 1,
          dist(idx  )(jdx-1) + 1,
          dist(idx-1)(jdx-1) + (if (s1(idx-1) == s2(jdx-1)) 0 else 1)
        )
      dist(s1.length)(s2.length)
    }

    def suggest(extr: String) = {
      for {
        k <- gold.keys;
        if stringDistance(k, extr) < extr.length / 2
      } yield ((k, gold(k)))
    }

    def promptScore(index: Int, extr: String, confidence: String, rest: Seq[Any]): Option[Boolean] = {
      println()
      System.out.println("Please score " + index + ": " + confidence + ":" + extr + ". (1/y/0/n/skip) ")
      if (rest.length > 0) println(rest.mkString("\t"))
      suggest(extr) foreach { case (k, v) =>
        println("suggest: " + v + "\t" + k)
      }
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
    } yield {
      scoreOption match {
        case Some(score) =>
          // update golden set
          golden += extr -> score
        case None =>
      }

      // output
      Scored(scoreOption, conf, extr, rest)
    }

    (scored.toList, golden)
  }
}

case class Scored(score: Option[Boolean], confidence: Double, extraction: String, extra: Seq[String]) {
  def toRow = (if (!score.isDefined) "" else if (score.get == true) "1" else "0")+"\t"+confidence+"\t"+extraction+"\t"+extra.mkString("\t")
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

    Scored(Some(score), confidence, extraction, extra)
  }
}

object GoldSet {
  def load(file: File) = {
    using(Source.fromFile(file, "UTF8")) { source =>
      source.getLines.map { line =>
        val parts = line.split("\t")
        parts(1) -> (if (parts(0) == "1") true else false)
      }.toMap
    }
  }

  def save(gold: Map[String, Boolean], file: File) = {
    using(new PrintWriter(file, "UTF8")) { writer =>
      gold.foreach { case (extr, correct) => writer.println((if (correct) 1 else 0) + "\t" + extr) }
    }
  }
}
