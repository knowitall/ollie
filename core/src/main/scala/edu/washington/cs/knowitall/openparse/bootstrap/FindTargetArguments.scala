package edu.washington.cs.knowitall.openparse.bootstrap

import scala.Array.canBuildFrom
import scala.collection.mutable
import scala.io.Source

import edu.washington.cs.knowitall.tool.stem.MorphaStemmer

import FindCommon.{zip3, proper, cleanArg}

/** Detemine valid arguments of extractions for the boostrap process. 
  * 
  * Only frequent proper arguments are used.
  * 
  * @author Michael Schmitz
  */
object FindTargetArguments {
  import FindCommon._

  val blacklist = Set("inc", "ltd", "page", 
    "vehicle", "turn", "site", "photo", "image", "gallery")

  def valid(lemma: String) = {
    lemma.length > 2 && lemma.length < 64 && !blacklist.contains(lemma)
  }

  /** Run over a file with four columns:
    * 
    *   string
    *   lemma
    *   postag
    *   count
    *
    * Count all of the proper arguments and print any arguments that
    * exceed the lower bound.  The lower bound is specified by the first
    * command-line argument. */
  def main(args: Array[String]) {
    val source = Source.fromFile(args(0), "UTF8")
    val lowerBound = args(1).toInt
    
    val map = new mutable.HashMap[String, Int]().withDefaultValue(0)
    for (line <- source.getLines) {
      try {
        val Array(string, lem, postag, count) = line.split("\t")
        // do our own normalization
        val lemma = string.split(" ").map(
          MorphaStemmer.instance.lemmatize(_)).mkString(" ")
        
        if (!string.contains("_")) {
          // remove DT
          val arg = cleanArg(
            zip3(
              postag.split("""\s+""").toList, 
              string.split("""\s+""").toList, 
              lemma.split("""\s+""").toList))
          val cleanLemma = arg.unzip3._3.mkString(" ")
          
          // make sure lemma is valid
          if (proper(postag.split(" ")) && valid(cleanLemma)) {
            map += cleanLemma -> (map(cleanLemma)+count.toInt)
          }
        }
      }
      catch {
        case e: MatchError =>
      }
    }
    
    source.close

    val keepers: List[(String, Int)] = (for ((k, v) <- map if v > lowerBound) yield {
      (k, v)
    })(scala.collection.breakOut)

    keepers.sortBy(_._2).reverse.foreach { case (k, v) => println(k + "\t" + v) }
  }
}
