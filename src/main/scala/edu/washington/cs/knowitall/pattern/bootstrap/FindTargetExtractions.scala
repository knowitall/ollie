package edu.washington.cs.knowitall
package parse
package pattern
package bootstrap

import scala.collection
import scala.collection.mutable
import scala.io.Source

import edu.washington.cs.knowitall.normalization._

object FindCommon {
  // tags allowed in proper arguments
  val properPostags = Set("DT", "IN", "NNP", "NNPS")
  def proper(lemmas: Array[String]) =
    lemmas.forall(properPostags.contains(_)) && lemmas.exists(lemma => lemma == "NNP" || lemma == "NNPS")
}

object FindTargetExtractions {
  import FindCommon._

  def zip3(l1 : List[String], l2 : List[String],l3 : List[String]) : List[(String, String, String)] =
  {
    def zip3$ (l1$ : List[String], l2$ : List[String], l3$ : List[String], acc : List[(String, String, String)]) : List[(String, String, String)] = l1$ match
    {
      case Nil => acc reverse
      case l1$head :: l1$tail => zip3$(l1$tail, l2$.tail, l3$.tail, (l1$head, l2$.head, l3$.head) :: acc)
    }

    zip3$(l1, l2, l3, List[(String,String,String)]())
  }

  def negated(lemmas: Array[String]) =
    lemmas.contains("not") || lemmas.contains("no") || lemmas.contains("n't") || lemmas.contains("never")

  /** The clean copy of ClueWeb should be piped in on stdin.
    * args(1): a file of target extractions
    * args(2): a file of permissible args */
  def main(args: Array[String]) {
    def stripPostag(target: String, part: Seq[(String, String, String)]) = {
      part.filter { case (pos, tok, lem) => !pos.matches(target) }
    }
    def stripLemma(target: String, part: Seq[(String, String, String)]) = {
      part.filter { case (pos, tok, lem) => !lem.matches(target) }
    }

    // read in the argument files
    val targets = Source.fromFile(args(0)).getLines.toList
    val arguments = Source.fromFile(args(1)).getLines.toSet

    // iterate over extractions
    for (line <- Source.stdin.getLines) {
      try {
        val Array(id, arg1String, relationString, arg2String, arg1Lemma, relationLemma, arg2Lemma, arg1Postag, relationPostag, arg2Postag, _, _, _, count, confidence, url, sentence) = line.split("\t", -1)
        val rs = new RelationString(relationString, relationLemma, relationPostag)
        rs.correctNormalization()
        val normalizedRelationString = rs.getNormPred

        val arg1 = zip3(arg1Postag.split("""\s+""").toList, arg1String.split("""\s+""").toList, arg1Lemma.split("""\s+""").toList)
        val rel = zip3(rs.getPosPred.split("""\s+""").toList, rs.getPred.split("""\s+""").toList, rs.getNormPred.split("""\s+""").toList)
        val arg2 = zip3(arg2Postag.split("""\s+""").toList, arg2String.split("""\s+""").toList, arg2Lemma.split("""\s+""").toList)

        implicit def t2mapper[A, B](t: (A, B)) = new { 
          def map[R](f: A => R, g: B => R) = (f(t._1), g(t._2)) 
        }

        val (arg1cleanpostag, arg1cleanString, arg1cleanLemma) = stripPostag("DT", arg1).unzip3
        val (arg2cleanpostag, arg2cleanString, arg2cleanLemma) = stripPostag("DT", arg2).unzip3
        val (relcleanPostag, relcleanString, relcleanLemma) = stripLemma("be", stripPostag("DT|IN|TO", rel)).unzip3

        // ensure the extraction parts are relatively small
        if (arg1Lemma.length < 64 && arg2Lemma.length < 64 && relationLemma.length < 64 && 
          // ensure the normalized relation string is a target
          targets.contains(normalizedRelationString) &&
          // ensure arguments are proper
          proper(arg1Postag.split("\\s+")) &&
          proper(arg2Postag.split("\\s+")) &&
          // ensure the args are permissible
          arguments.contains(arg1cleanLemma.mkString(" ")) && arguments.contains(arg2cleanLemma.mkString(" ")) &&
          // ensure the unnormalized relation is not negated
          !negated(relationLemma.split(" "))) {

          for (i <- 0 until count.toInt) {
            println(Iterable(normalizedRelationString, arg1cleanLemma.mkString(" "), arg2cleanLemma.mkString(" "), (arg1cleanLemma ++ relcleanLemma ++ arg2cleanLemma).mkString(" "), arg1String, relationString, arg2String, arg1Postag, relationPostag, arg2Postag).mkString("\t"))
          }
        }
      }
      catch {
        case e => e.printStackTrace
      }
    }
  }
}

object FindTargetArguments {
  import FindCommon._

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
    val source = Source.stdin
    val lowerBound = args(0).toInt
    val map = new mutable.HashMap[String, Int]().withDefaultValue(0)
    for (line <- source.getLines) {
      try {
        val Array(count, string, lemma, postag) = line.split("\t")
        if (proper(postag.split(" "))) {
          map += lemma -> (map(lemma)+count.toInt)
        }
      }
      catch {
        case e: MatchError =>
      }
    }

    val keepers: List[(String, Int)] = (for ((k, v) <- map if v > lowerBound) yield {
      (k, v)
    })(collection.breakOut)

    keepers.sortBy(_._2).reverse.foreach { case (k, v) => println(k + "\t" + v) }
  }
}
