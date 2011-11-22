package edu.washington.cs.knowitall.pattern.lda

import java.io._
import scala.io.Source

class Distributions(relp: Array[Array[Int]], relt: Array[Array[Int]], val relationDecoding: Map[Int, String], val patternDecoding: Map[Int, String]) {
  val relationEncoding = relationDecoding.map(_.swap)
  val patternEncoding = patternDecoding.map(_.swap)

  // count relations
  val relationCount = relp.size
  def relationCount(r: Int) = relt(r).size

  // count topics
  val topicCount = relt.map(_.max).reduce(math.max(_, _)) + 1
  val topicCountMap = ((0 until topicCount).map(z => (z, relt.map(_.filter(_ == z).size).sum))).toMap
  def topicCount(z: Int): Int = topicCountMap(z)

  // count patterns (unoptimized)
  val patternCount = relp.map(_.max).reduce(math.max(_, _)) + 1

  // laplacian smoothing constant
  val smoothing = 1.0

  // topics by patterns
  val top_pat = Array.tabulate(topicCount)(topic => buildPatterns(topic))
  // relations by topics
  val rel_top = Array.tabulate(relationCount){ relation => 
    buildMapCount(relt(relation))
  }
  // relations by patterns
  val rel_pat = Array.tabulate(relationCount) { relation =>
    buildMapCount(relp(relation))
  }

  /*
   * build a row to store the counts of a particular pattern and all patterns
   */
  def buildPatterns(topic: Int): (Map[Int, Int], Int) = {
    val patterns = for (
      r <- 0 until relationCount; 
      i <- 0 until relt(r).size if relt(r)(i) == topic) yield {
      relp(r)(i)
    }

    buildMapCount(patterns)
  }

  /* 
   * build a row to store the counts of an item (part 1 of the tuple)
   * and all items( part 2 of the tuple) */
  def buildMapCount(row: Seq[Int]): (Map[Int, Int], Int) =
    (row.groupBy(x=>x).map { case(a, b) => (a, b.size) }.withDefaultValue(0), row.size)

  /* 
   * compute the beta function for the LDA results.
   * beta = P(p | z) */
  def beta(topic: Int)(pattern: Int): Double = {
    var numcount = 0
    var dencount = 0

    val (patterns, patternSize) = top_pat(topic)

    val count = patterns(pattern)
    val num = count.toDouble + smoothing
    val den = patternSize.toDouble + (smoothing * patternCount)

    num.toDouble / den.toDouble
  }

  /*
   * compute the beta counts for the LDA results.  This is useful when
   * comparing to the LDA's output. */
  def betac(topic: Int)(pattern: Int): Int = {
    val (patterns, patternSize) = top_pat(topic)
    patterns(pattern)
  }

  /*
   * Compute the probability of a topic given a relation P(z | r) */
  def theta(relation: Int)(topic: Int): Double = {
    val (topics, topicSize) = rel_top(relation)

    val num = topics(topic).toDouble + smoothing
    val den = topicSize.toDouble + (smoothing * topicCount)

    num.toDouble / den.toDouble
  }

  /*
   * compute the theta counts for the LDA results.  This is useful when
   * comparing to the LDA's output. */
  def thetac(relation: Int)(topic: Int): Int = {
    val (topics, topicSize) = rel_top(relation)
    topics(topic)
  }

  def probs(relation: Int): Double = {
    var sum = 0.0
    for (p <- 0 until patternCount) {
      val x = prob(relation)(p)
      sum += x
    }

    sum
  }

  /*
   * compute P(p | r) from the resulting distributions of the LDA */
  def prob(relation: Int)(pattern: Int): Double = {
    (0 until topicCount).foldLeft(0.0) { (total, z) =>
      val t = theta(relation)(z)
      val b = beta(z)(pattern)
      total + t*b
    }
  }

  def prob(relation: String)(pattern: String): Double =
    prob(relationEncoding(relation))(patternEncoding(pattern))

  /*
   * compute P(p | r) directly from the input data. */
  def probBaseline(relation: Int)(pattern: Int): Double = {
    val (patterns, size) = rel_pat(relation)

    val num = patterns(pattern)
    val den = size

    num.toDouble / den.toDouble
  }
}

object Distributions {
  def fromArgs(args: Array[String]) = {
    val (relPattern, relTopic, relationDecoding, patternDecoding) =
      Distributions.load(args)
    new Distributions(relPattern, relTopic, relationDecoding, patternDecoding)
  }

  def load(args: Array[String]) = {
    val Array(ldaInputPath, ldaOutputPath, relationDecodingPath, patternDecodingPath) = args

    val ldaInput = Source.fromFile(new File(ldaInputPath))
    val relPattern = Decode.readRelFile(ldaInput)
    ldaInput.close

    val ldaOutput = Source.fromFile(new File(ldaOutputPath))
    val relTopic = Decode.readRelFile(ldaOutput)
    ldaOutput.close

    val relationDecodingSource = Source.fromFile(new File(relationDecodingPath))
    val relationDecoding = Decode.readDecoding(relationDecodingSource)
    relationDecodingSource.close

    // index patterns from 0
    val patternDecodingSource = Source.fromFile(new File(patternDecodingPath))
    val patternDecoding = Decode.readDecoding(patternDecodingSource).map{ case (a, b) => (a-1, b) } 
    patternDecodingSource.close

    (relPattern, relTopic, relationDecoding, patternDecoding)
  }

  /*
   * args:
   *   1.  Directory that holds LDA files.
   *   2.  Path to LDA output file. 
   *   3.  Directory for output.
   */
  def main(args: Array[String]) = {
    val prefix = args(0)
    val outputPath = args(1)

    println("loading input files...")
    val inputFiles = Array(
        prefix + "/ldainput/rel_pattern.txt", 
        prefix + "/ldaoutput/output.txt", 
        prefix + "/ldainput/relation_encoding.txt", 
        prefix + "/ldainput/pattern_encoding.txt")
    println(inputFiles.map(" " * 4 + _).mkString("\n"))
    val dist = Distributions.fromArgs(inputFiles)

    val probabilitiesOutputPath = outputPath + "/probabilities.txt"
    println("computing probabilities ("+probabilitiesOutputPath+")...")
    val probabilitiesOutput = new PrintStream(
      new FileOutputStream(probabilitiesOutputPath))
    Probabilities.probabilities(dist, probabilitiesOutput)
    probabilitiesOutput.close

    val byTopicOutputPath = outputPath + "/bytopic.txt"
    println("creating summary by topic ("+byTopicOutputPath+")...")
    val bytopicOutput = new PrintStream(
      new FileOutputStream(byTopicOutputPath))
    ByTopic.byTopic(dist, bytopicOutput)
    bytopicOutput.close

    val evaluateOutputPath = outputPath + "/evaluate.txt"
    println("evaluating test set ("+evaluateOutputPath+")...")
    val evaluateOutput = new PrintStream(
      new FileOutputStream(evaluateOutputPath))
    Evaluate.evaluate(dist, Source.fromFile(prefix + "/raw/test.txt"), evaluateOutput)
    evaluateOutput.close

    val testDistributionsOutputPath = outputPath + "/testdist.txt"
    println("testing distributions ("+testDistributionsOutputPath+")...")
    val testDistributionsOutput = new PrintStream(
      new FileOutputStream(testDistributionsOutputPath))
    TestDistributions.test(dist, testDistributionsOutput)
    testDistributionsOutput.close
  }
}

object Probabilities {
  def probabilities(dist: Distributions, output: PrintStream) {
    output.println("\t" + (0 until dist.patternCount).mkString("\t"))
    for (r <- 0 until dist.relationCount) {
      output.println (dist.relationDecoding(r) + "\t" +
        (
          for (p <- 0 until dist.patternCount) yield {
            "%.4f".format(dist.prob(r)(p))
          }
        ).mkString("\t"))
    }
  }

  // compute P(p | r) for all p, r based on the output from the LDA
  def main(args: Array[String]) {
    val dist = Distributions.fromArgs(args)
    probabilities(dist, System.out)
  }
}

// used for checking that our computations match HBC's
object LDAFormatDistributions {
  def main(args: Array[String]) {
    val dist = Distributions.fromArgs(args)

    println("theta")
    for (r <- 0 until dist.relationCount) {
    println(
        (
          for (z <- 0 until dist.topicCount) yield {
            dist.thetac(r)(z)
          }
        ).mkString(" "))
    }

    println("beta")
    for (z <- 0 until dist.topicCount) {
    println(
        (
          for (p <- 0 until dist.patternCount) yield {
            dist.betac(z)(p)
          }
        ).mkString(" "))
    }
  }
}

// make sure all the distributions sum to 1
object TestDistributions {
  def test(dist: Distributions, output: PrintStream) {
    output.println("relations: " + dist.relationCount)
    output.println("patterns: " + dist.patternCount)
    output.println("topics: " + dist.topicCount)

    val epsilon = .01

    for (r <- 0 until dist.relationCount) {
      val v = Iterable.tabulate(dist.topicCount){dist.theta(r)(_)}.sum
      output.println("theta = P(z | r="+r+") = " + v)
      if (v < 1.0-epsilon || v > 1.0+epsilon) sys.exit
    }

    for (z <- 0 until dist.topicCount) {
      val v = Iterable.tabulate(dist.patternCount){dist.beta(z)(_)}.sum
      output.println("beta = P(p | z="+z+") = " + v)
      if (v < 1.0-epsilon || v > 1.0+epsilon) sys.exit
    }

    for (r <- 0 until dist.relationCount) {
      val v = Iterable.tabulate(dist.patternCount){dist.prob(r)(_)}.sum
      output.println("P(p | r="+r+") = " + v)
      if (v < 1.0-epsilon || v > 1.0+epsilon) sys.exit
    }

    output.println("tests complete.")
  }

  def main(args: Array[String]) {
    val dist = Distributions.fromArgs(args)
    test(dist, System.out)
  }
}

object Evaluate {
  class ExampleException(message: String) extends Exception(message) {}
  def evaluate(dist: Distributions, examples: Source, output: PrintStream) = {
    def printResult(name: String, prob: Double, relation: Int, pattern: Int) {
      output.println(name + ": "+(" "*(5-name.size))+"%2f".format(prob)+"\t"+relation+"\t"+pattern+"\t"+dist.relationDecoding(relation)+"\t"+dist.patternDecoding(pattern))
    }

    val random = new java.util.Random

    var ldaGtRand = 0
    var baseGtRand = 0
    var total = 0
    for (line <- examples.getLines) {
      try {
        val Array(relation, arg1, arg2, pattern, slot1, slot2) = line.split("\t", -1)
        val randRelation = random.nextInt(dist.relationCount)
        val randPattern = random.nextInt(dist.patternCount)

        val r = dist.relationEncoding(relation)
        val p = dist.patternEncoding.getOrElse(pattern, throw new ExampleException("test set contains untrained pattern: " + pattern))

        val lda = dist.prob(r)(p)
        printResult("lda", lda, r, p)
        // println("set:  "+"%2f".format(lda)+"\t"+relationEncoding(relation)+"\t"+patternEncoding(pattern)+"\t"+relation+"\t"+pattern)

        val base = dist.probBaseline(r)(p)
        printResult("base", base, r, p)
        //println("base: "+"%2f".format(base)+"\t"+relationEncoding(relation)+"\t"+patternEncoding(pattern)+"\t"+relation+"\t"+pattern)

        val randl = dist.prob(dist.relationEncoding(relation))(randPattern)
        printResult("randl", randl, r, randPattern)

        val randb = dist.probBaseline(dist.relationEncoding(relation))(randPattern)
        printResult("randb", randb, r, randPattern)

        output.println("lda/randl:"+"%2f".format(lda / randl))
        output.println("base/randb:"+"%2f".format(lda / randb))
        output.println()

        total += 1
        if (lda > randl) ldaGtRand += 1
        if (base > randb) baseGtRand += 1
      }
      catch {
        case e: ExampleException => System.err.println("error: " + e)
      }
    }

    output.println("lda  > randl: " + ldaGtRand)
    output.println("base > randb: " + baseGtRand)
    output.println("total: " + total)
  }

  def main(args: Array[String]) {
    val dist = Distributions.fromArgs(args)
    evaluate(dist, Source.stdin, System.out)
  }
}

object ByTopic {
  def main(args: Array[String]) {
    val dist = Distributions.fromArgs(args)
    byTopic(dist, System.out)
  }

  def byTopic(dist: Distributions, output: PrintStream) {
    val relationsWithZHigh =
    for (z <- 0 until dist.topicCount) yield {
      val reverseThetas = 
      for (r <- 0 until dist.relationCount) yield {
        (r+"/ "+dist.relationDecoding(r), "%.4f".format(dist.theta(r)(z)))
      }

      (z, reverseThetas.sortBy(_._2).reverse.take(5))
    }

    val topRelations =
    for (z <- 0 until dist.topicCount) yield {
      val reverseThetas = 
      for (r <- 0 until dist.relationCount) yield {
        (r+"/ "+dist.relationDecoding(r), "%.4f".format(dist.theta(r)(z) * dist.relationCount(r) / dist.topicCount(z)))
      }

      (z, reverseThetas.sortBy(_._2).reverse.take(5))
    }

    val topPatterns =
    for (z <- 0 until dist.topicCount) yield {
      val betas = 
      for (p <- 0 until dist.patternCount) yield {
        (p+"/ "+dist.patternDecoding(p), "%.4f".format(dist.beta(z)(p)))
      }

      (z, betas.sortBy(_._2).reverse.take(5))
    }

    output.println("Best relations with z as a topic (optimizing P(z | r)).")
    output.println("Best relations for topic z (optimizing P(r | z)).")
    output.println("Best patterns for topic z (optimizing beta = P(p | z))")
    for (results <- List(relationsWithZHigh, topRelations, topPatterns).transpose;
      (z, xs) <- results) {
      output.println(z + ": " + xs.mkString(", "))
    }
  }
}
