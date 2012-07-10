package edu.washington.cs.knowitall.openparse.bootstrap

import scala.io.Source

import org.slf4j.LoggerFactory

/** Filter the target extractions.  We only want to keep extractions that
  * occur more than once and have a relation with more than 15 seeds.
  * 
  * @author Michael Schmitz
  */
object FilterTargetExtractions {
  val logger = LoggerFactory.getLogger(this.getClass)
  
  final val MIN_RELATION_SEEDS = 15

  def main(args: Array[String]) {
    val inputFile = Source.fromFile(args(0), "UTF8")

    logger.info("reading lines and counting")
    var relationCounts = Map[String, Int]().withDefaultValue(0)
    var seedCounts = Map[(String, String, String, String), Int]().withDefaultValue(0)
    for (line <- inputFile.getLines) {
      val Array(rel, arg1, arg2, lemmas, _*) = line.split("\t")

      val seed = (rel, arg1, arg2, lemmas)

      // make sure the relation contains at least on of the lemmas
      // this excludes, for example, "be in"
      if (rel.split(" ").exists (lemmas contains _)) {
        seedCounts += seed -> (seedCounts(seed) + 1)
        relationCounts += rel -> (relationCounts(rel) + 1)
      }
    }

    // keep relations with more than 15 seeds
    // and more than 0 lemmas
    val relations: Set[String] =
      (for {
        (rel, count) <- relationCounts;
        if (count > MIN_RELATION_SEEDS)
      } yield (rel))(scala.collection.breakOut)
    logger.info("keeping " + relations.size + "/" + relationCounts.size + " relations")

    // keep seeds that occur more than once
    val seeds =
      for {
        (seed @ (rel, arg1, arg2, lemmas), count) <- seedCounts;
        if count > 1 && relations.contains(rel)
      } yield (seed)

    logger.info("keeping " + seeds.size + "/" + seedCounts.size + " seeds")

    logger.info("printing seeds to keep")
    for (seed <- seeds) {
      println(seed.productIterator.mkString("\t"))
    }
  }
}
