package edu.washington.cs.knowitall
package pattern
package bootstrap

import org.slf4j.LoggerFactory

import scopt.OptionParser

import scala.collection
import scala.collection.mutable
import scala.io.Source

import tool.stem.MorphaStemmer

import edu.washington.cs.knowitall.normalization._

object FindTargetExtractions {
  val logger = LoggerFactory.getLogger(this.getClass)
  import FindCommon._

  def negated(lemmas: Array[String]) =
    lemmas.contains("not") || lemmas.contains("no") || lemmas.contains("n't") || lemmas.contains("never")

  /** args(0): extractions formatted like the clean clueweb
    * args(1): a file of target relations
    * args(2): a file of permissible args */
  def main(args: Array[String]) {

    val parser = new OptionParser("findextr") {
      var extractionFilePath: String = _
      var relationFilePath: String = _
      var argumentFilePath: String = _

      opt("e", "extractions", "<file>", "extraction file", { v: String => require(v != null); extractionFilePath = v })
      opt("r", "relations", "<file>", "relation file", { v: String => require(v != null); relationFilePath = v })
      opt("a", "arguments", "<file>", "argument file", { v: String => require(v != null); argumentFilePath = v })
    }

    if (parser.parse(args)) {
      // read in the argument files
      val extractions = Source.fromFile(parser.extractionFilePath)
      logger.info("loading targets")
      val relationsRows = Source.fromFile(parser.relationFilePath).getLines.map(line => line.split("\t")).toList
      val targets = relationsRows.map(_(0))
      val relationLemmas = relationsRows.map(row => (row(0), row(1).split(" "))).toMap
      logger.info("5 targets: " + targets.take(5).mkString(", "))
      logger.info("loading arguments")
      val arguments = Source.fromFile(parser.argumentFilePath).getLines.map(line => line.split("\t")(0)).toSet
      logger.info("5 arguments: " + arguments.take(5).mkString(", "))

      // iterate over extractions
      logger.info("iterating over extractions")
      for (line <- extractions.getLines) {
        try {
          val Array(id, arg1String, relationString, arg2String, _, relationLemma, _, arg1Postag, relationPostag, arg2Postag, _, _, _, count, confidence, url, sentence) = line.split("\t", -1)
          val arg1Lemma = arg1String.split(" ").map(MorphaStemmer.instance.lemmatize(_)).mkString(" ")
          val arg2Lemma = arg2String.split(" ").map(MorphaStemmer.instance.lemmatize(_)).mkString(" ")
          val rs = new RelationString(relationString, relationLemma, relationPostag)
          rs.correctNormalization()

          val arg1 = zip3(arg1Postag.split("""\s+""").toList, arg1String.split("""\s+""").toList, arg1Lemma.split("""\s+""").toList)
          val rel = zip3(rs.getPosPred.split("""\s+""").toList, rs.getPred.split("""\s+""").toList, rs.getNormPred.split("""\s+""").toList)
          val arg2 = zip3(arg2Postag.split("""\s+""").toList, arg2String.split("""\s+""").toList, arg2Lemma.split("""\s+""").toList)

          implicit def t2mapper[A, B](t: (A, B)) = new { 
            def map[R](f: A => R, g: B => R) = (f(t._1), g(t._2)) 
          }

          val (arg1cleanPostags, arg1cleanStrings, arg1cleanLemmas) = cleanArg(arg1).unzip3
          val (arg2cleanPostags, arg2cleanStrings, arg2cleanLemmas) = cleanArg(arg2).unzip3
          val (relcleanPostags, relcleanStrings, relcleanLemmas) = stripPostag("DT", rel).unzip3

          val relcleanLemmaString = relcleanLemmas.mkString(" ")

          // ensure the extraction parts are relatively small
          if (relationLemma.length < 64 && 
            // ensure the normalized relation string is a target
            targets.contains(relcleanLemmaString) &&
            // ensure arguments are proper
            proper(arg1Postag.split("\\s+")) &&
            proper(arg2Postag.split("\\s+")) &&
            // ensure the args are permissible
            arguments.contains(arg1cleanLemmas.mkString(" ")) && arguments.contains(arg2cleanLemmas.mkString(" ")) &&
            // ensure the unnormalized relation is not negated
            !negated(relationLemma.split(" "))) {

            for (i <- 0 until count.toInt) {
              println(Iterable(
                relcleanLemmaString, 
                arg1cleanLemmas.mkString(" "), 
                arg2cleanLemmas.mkString(" "),
                (arg1cleanLemmas ++ relationLemmas(relcleanLemmaString) ++ arg2cleanLemmas).mkString(" "), 
                arg1String, relationString, arg2String, arg1Postag, relationPostag, arg2Postag).mkString("\t"))
            }
          }
        }
        catch {
          case e => // e.printStackTrace
        }
      }
    }
  }
}
