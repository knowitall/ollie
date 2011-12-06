package edu.washington.cs.knowitall
package pattern
package bootstrap

import scala.io.Source
import org.slf4j.LoggerFactory

object ReduceTargetExtractions {
  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]) {
    val inputFile = Source.fromFile(args(0))

    logger.info("reading lines and counting")
    var relationCounts = Map[String, Int]().withDefaultValue(0)
    var seedCounts = Map[(String, String, String, String), Int]().withDefaultValue(0)
    for (line <- inputFile.getLines) {
      val Array(rel, arg1, arg2, lemmas, _*) = line.split("\t")

      val seed = (rel, arg1, arg2, lemmas)
      seedCounts += seed -> (seedCounts(seed) + 1)

      relationCounts += rel -> (relationCounts(rel) + 1)
    }

    // keep relations with more than 15 seeds
    val relations = 
      (for ((rel, count) <- relationCounts; if (count > 15)) yield (rel)).toSet
    logger.info("keeping " + relations.size + "/" + relationCounts.size + " relations")

    // keep seeds that occur more than once
    logger.info("printing seeds to keep")
    for ((seed @ (rel, arg1, arg2, lemmas), count) <- seedCounts; 
      if count > 1 && relations.contains(rel)) {
      println(seed.productIterator.mkString("\t"))
    }
  }
}
