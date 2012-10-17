package edu.washington.cs.knowitall.ollie.confidence.train

import java.io.File

import scala.io.Source

import edu.washington.cs.knowitall.common.Analysis
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.ollie.ScoredOllieExtractionInstance
import edu.washington.cs.knowitall.ollie.confidence.OllieFeatureSet
import edu.washington.cs.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import scopt.mutable.OptionParser

object CrossValidateConfidence {
  def main(args: Array[String]) {
    object settings extends Settings {
      var inputFile: File = _
      var outputFile: Option[File] = None
    }

    val parser = new OptionParser("scoreextr") {
      arg("labelled", "labelled extractions", { path: String => settings.inputFile = new File(path) })
      argOpt("output", "output file", { path: String => settings.outputFile = Some(new File(path)) })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }

   abstract class Settings {
     def inputFile: File
     def outputFile: Option[File]

     val splits = 10
   }


  def run(settings: Settings) = {
    val trainer = new BreezeLogisticRegressionTrainer(OllieFeatureSet)

    val data =
      using (Source.fromFile(settings.inputFile)) { source =>
        (source.getLines map (ScoredOllieExtractionInstance.tabDeserialize)).toList
      }

    val splits = data.iterator.sliding(data.size / settings.splits, data.size / settings.splits).withPartial(false)
    val results = for {
      split <- splits.toList

      val test = split
      val training = data filterNot (test contains _)

      val classifier = trainer.train(training)
    } yield {
      for (example <- test) yield {
        val conf = classifier.apply(example.inst)
        val correct =
          if (conf >= 0.5 && example.score) true
          else if (conf < 0.5 && !example.score) true
          else false
        (conf, correct)
      }
    }

    val pys = results.map { list =>
      val py = Analysis.precisionYield(list.sortBy(-_._1).map(_._2))

      py
    }

    val aucs = pys.zipWithIndex map { case (py, i) =>
      println("Split " + i)
      py foreach { case (y, p) =>
        println(Iterable(y.toString, "%1.4f" format p).mkString("\t"))
      }

      val auc = Analysis.areaUnderCurve(py)
      println("auc: " + auc)

      println()
      auc
    }

    var auc = breeze.linalg.mean(aucs)
    println("avg auc: " + auc)
  }
}