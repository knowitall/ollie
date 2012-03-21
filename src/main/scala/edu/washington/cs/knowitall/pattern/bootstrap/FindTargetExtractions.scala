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
  import FindCommon._

  val logger = LoggerFactory.getLogger(this.getClass)

  def negated(lemmas: Array[String]) =
    lemmas.contains("not") || lemmas.contains("no") || lemmas.contains("n't") || lemmas.contains("never")

  val lemmaBlacklist = Set("the", "that", "of")

  def main(args: Array[String]) {

    val parser = new OptionParser("findextr") {
      var extractionFilePath: String = _
      var relationFilePath: Option[String] = None
      var argumentFilePath: String = _

      arg("extractions", "extraction file", { v: String => require(v != null); extractionFilePath = v })
      arg("arguments", "argument file", { v: String => require(v != null); argumentFilePath = v })
      opt("r", "relations", "<file>", "relation file", { v: String => require(v != null); relationFilePath = Some(v) })
    }

    if (parser.parse(args)) {
      // read in the argument files
      val extractions = Source.fromFile(parser.extractionFilePath, "UTF8")
      logger.info("loading targets")
      val relationsRows = parser.relationFilePath.map(Source.fromFile(_, "UTF8").getLines.map(line => line.split("\t")).toList)
      val targets = relationsRows.map(_ map (_(0)))
      val relationLemmaLookup = relationsRows.map(_.map(row => (row(0), row(1).split(" "))).toMap)
      def relationLemmas(relation: String): Seq[String] = {
        relationLemmaLookup match {
          case Some(lookup) => lookup(relation)
          case None => relation.split(" ") filterNot OpenParse.LEMMA_BLACKLIST
        }
      }

      targets match {
        case Some(targets) => logger.info("5 targets: " + targets.take(5).mkString(", "))
        case None => logger.info("No target restriction")
      }
      logger.info("loading arguments")
      val arguments = Source.fromFile(parser.argumentFilePath, "UTF8").getLines.map(line => line.split("\t")(0)).toSet
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
          val (relcleanPostags, relcleanStrings, relcleanLemmas) = {
            val stripped = stripPostag("RB.*", stripPostag("DT", rel))
            val beIndex = rel.indexWhere(_._3 == "be")
            val penultimateAdjective = 
              if (rel.length - beIndex >= 3 && (rel.drop(beIndex).head._3 startsWith "be") && rel.last._1 == "IN") {
                // return the penultimate if it's VERB ADJECTIVE PREPOSITION
                Some(rel.init.last)
              }
              else None
            
            (stripPostag("JJS?".r, stripped) ++ penultimateAdjective).unzip3
          }

          val relcleanLemmaString = relcleanLemmas.mkString(" ")
          val arg1cleanLemmaString = arg1cleanLemmas.mkString(" ")
          val arg2cleanLemmaString = arg2cleanLemmas.mkString(" ")

          // ensure the extraction parts are relatively small
          if (relationLemma.length < 64 && 
            // ensure the normalized relation string is a target
            targets.map(_ contains relcleanLemmaString).getOrElse(true) &&
            // ensure arguments are proper
            (proper(arg1Postag.split("\\s+")) ||
            proper(arg2Postag.split("\\s+"))) &&
            arg1cleanLemmaString != arg2cleanLemmaString &&
            // ensure the args are permissible
            arguments.contains(arg1cleanLemmaString) && arguments.contains(arg2cleanLemmaString) &&
            // ensure the unnormalized relation is not negated
            !negated(relationLemma.split(" "))) {

            val lemmas = (arg1cleanLemmas ++ relationLemmas(relcleanLemmaString) ++ arg2cleanLemmas) filterNot lemmaBlacklist

            for (i <- 0 until count.toInt) {
              println(Iterable(
                relcleanLemmaString, 
                arg1cleanLemmaString, 
                arg2cleanLemmaString,
                lemmas.mkString(" "), 
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
