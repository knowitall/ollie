package edu.washington.cs.knowitall.ollie

import edu.washington.cs.knowitall.tool.conf.Labelled

/** OllieExtractionInstance represents a boolean score coupled with
  * an extraciton instance.
  *
  * @param  score  the label for this extraction
  * @param  inst  the extraction instance labelled
  */
class ScoredOllieExtractionInstance(
    val score: Boolean,
    val inst: OllieExtractionInstance) extends Labelled[OllieExtractionInstance](score, inst) {

  override def toString = score + ":" + inst.extr

  def tabSerialize: String = {
    Iterable(if (score) 1 else 0, inst.extr.toString, inst.tabSerialize).mkString("\t")
  }
}

object ScoredOllieExtractionInstance {
  def tabDeserialize(string: String): ScoredOllieExtractionInstance = {
    try {
      val Array(scoreString, _, rest @ _*) = string.split('\t')

      val score =
        if (scoreString == "1") true
        else if (scoreString == "0") false
        else throw new IllegalArgumentException("bad score: " + scoreString)
      val (inst, r2) = OllieExtractionInstance.tabDeserialize(rest)

      require(r2.isEmpty)

      new ScoredOllieExtractionInstance(score, inst)
    } catch {
      case e => throw new IllegalArgumentException("could not tab deserialize: " + string, e)
    }
  }

  val numFinder = "[0-9]+".r
}
