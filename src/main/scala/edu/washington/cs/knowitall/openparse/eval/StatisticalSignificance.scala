package edu.washington.cs.knowitall.openparse.eval

import java.io.File

import edu.washington.cs.knowitall.common.{Random, Analysis}

import scopt.OptionParser

object StatisticalSignificance {
  abstract class Settings {
    def iterations: Int
    def systemFile: File
    def baselineFile: File
  }

  def main(args: Array[String]) {
    object settings extends Settings {
      var systemFile: File = _
      var baselineFile: File = _
      var iterations: Int = 1000
    }

    val parser = new OptionParser("statsig") {
      arg("system", "scored extractions from the new system", { path: String => settings.systemFile = new File(path) })
      arg("baseline", "scored extractions from the baseline system", { path: String => settings.baselineFile = new File(path) })
      intOpt("i", "iterations", "number of iterations", { n: Int => settings.iterations = n })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }

  /**
    * Uses the bootstrap test for statistical significance.  
    * This is described in the following paper:
    *
    *    http://maroo.cs.umass.edu/pub/web/getpdf.php?id=744
    *
    * Note that this function is agnostic to the order of 
    * `system` and `baseline`.
    * 
    * @param  system  a metric for the system, i.e. AUC
    * @param  baseline  a metric for the baseline, i.e. AUC
    * @param  sample  a lambda that resamples the systems, returning the metric, i.e. AUC
    * @param  iterations  the number of iterations
    */
  def bootstrapTestWithMetric(system: Double, 
      baseline: Double, 
      sample: ()=>(Double, Double), 
      iterations: Int) = {
    val difference = math.abs(system - baseline)
    val sampled = for (i <- 0 until iterations) yield (sample())
    val differences = sampled.map { case (sys, base) => math.abs(sys - base) }
    val average = differences.sum / differences.size.toDouble
    val normed = differences.map(_ - average)
    val pscore = normed.count(_ >= difference).toDouble / normed.size.toDouble

    pscore
  }

  /**
    * Uses the bootstrap test for statistical significance.  
    * This is described in the following paper:
    *
    *    http://maroo.cs.umass.edu/pub/web/getpdf.php?id=744
    *
    * Note that this function is agnostic to the order of 
    * `system` and `baseline`.
    * 
    * @param  system  a metric for the system, i.e. AUC
    * @param  baseline  a metric for the baseline, i.e. AUC
    * @param  sample  a lambda that resamples the systems, returning the metric, i.e. AUC
    * @param  iterations  the number of iterations
    */
  def bootstrapTestWithScores(system: Seq[Boolean], 
    baseline: Seq[Boolean], 
    metric: Seq[Boolean]=>Double, 
    iterations: Int, rand: util.Random) = {

    def sample(extrs: Seq[Boolean]) = 
      metric(extrs.map(extr=>Random.choose(extrs, extrs.size, rand)))

    bootstrapTestWithMetric(metric(system), metric(baseline), 
        ()=>(sample(system), sample(baseline)), iterations)
  }

  def run(settings: Settings) {
    val rand = new util.Random

    def areaUnderCurve(scoreds: Seq[Scored]) = {
      val points = Analysis.precisionYieldMeta(scoreds.map(extr => (extr.confidence, extr.score.get)))
      Analysis.areaUnderCurve(points.map { case (conf, yld, prc) => (yld, prc) })
    }

    val systemExtractionsAll: Seq[Scored] =
      Score.loadScoredFile(settings.systemFile).sortBy(-_.confidence)
    val baselineExtractionsAll: Seq[Scored] = 
      Score.loadScoredFile(settings.baselineFile).sortBy(-_.confidence)

    val sentences = (systemExtractionsAll.map(_.extra(0)).toSet ++ baselineExtractionsAll.map(_.extra(0)).toSet).toSeq.take(50).toSet

    val systemExtractions = systemExtractionsAll.filter(extr => sentences.contains(extr.extra(0)))
    val baselineExtractions = baselineExtractionsAll.filter(extr => sentences.contains(extr.extra(0)))

    def sample(): (Double, Double) = {
      def helper(extrs: Seq[Scored]) = {
        val sent = sentences.map(extr=>Random.choose(sentences, sentences.size, rand))
        // val set = extrs.map(extr=>Random.choose(extrs, extrs.size, rand)).sortBy(-_.confidence)
        val set = sent.flatMap(sent => extrs.filter(sent == _.extra(0))).toSeq.sortBy(_.confidence)
        val auc = areaUnderCurve(set)
        auc
      }

      (helper(systemExtractions), helper(baselineExtractions))
    }

    /*
    val metric = (scores: Seq[Boolean])=>Analysis.areaUnderCurve(Analysis.precisionYield(scores))
    val pscore = bootstrapTest(systemExtractions.map(_.score.get), baselineExtractions.map(_.score.get),
         metric, settings.iterations, rand)
         */
    val pscore = bootstrapTestWithMetric(
      areaUnderCurve(systemExtractions),
      areaUnderCurve(baselineExtractions),
      sample, settings.iterations)

    println(pscore)
  }
}
