package edu.washington.cs.knowitall
package parse
package pattern
package bootstrap

import scala.io.Source

import edu.washington.cs.knowitall.normalization._

object FindTargetExtractions {
  def zip3(l1 : List[String], l2 : List[String],l3 : List[String]) : List[(String, String, String)] =
  {
    def zip3$ (l1$ : List[String], l2$ : List[String], l3$ : List[String], acc : List[(String, String, String)]) : List[(String, String, String)] = l1$ match
    {
      case Nil => acc reverse
      case l1$head :: l1$tail => zip3$(l1$tail, l2$.tail, l3$.tail, (l1$head, l2$.head, l3$.head) :: acc)
    }

    zip3$(l1, l2, l3, List[(String,String,String)]())
  }

  def main(args: Array[String]) {
    def stripPostag(target: String, part: Seq[(String, String, String)]) = {
      part.filter { case (pos, tok, lem) => !pos.matches(target) }
    }
    def stripLemma(target: String, part: Seq[(String, String, String)]) = {
      part.filter { case (pos, tok, lem) => !lem.matches(target) }
    }

    val proper = Set("DT", "NNP", "NNPS")

    val targets = Source.fromFile(args(0)).getLines.toList

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

        if (arg1Lemma.length < 64 && arg2Lemma.length < 64 && relationLemma.length < 64 && 
          targets.contains(normalizedRelationString) &&
          arg1Postag.split("\\s+").forall(proper.contains(_)) && 
          arg2Postag.split("\\s+").forall(proper.contains(_))) {
          for (i <- 0 until count.toInt) {
            System.out.println(Iterable(relationLemma, arg1cleanLemma.mkString(" "), arg2cleanLemma.mkString(" "), (arg1cleanLemma ++ relcleanLemma ++ arg2cleanLemma).mkString(" "), arg1String, relationString, arg2String, arg1Postag, relationPostag, arg2Postag).mkString("\t"))
          }
        }
      }
      catch {
        case e => e.printStackTrace
      }
    }
  }
}
