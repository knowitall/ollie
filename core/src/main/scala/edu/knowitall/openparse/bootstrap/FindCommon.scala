package edu.knowitall.openparse.bootstrap

import scala.util.matching.Regex

/** Common functionality for bootstrap code.
  * 
  * @author Michael Schmitz
  */
object FindCommon {
  // tags allowed in proper arguments
  val properPostags = Set("DT", "IN", "NNP", "NNPS")
  def proper(lemmas: Array[String]) =
    lemmas.forall(properPostags.contains(_)) && lemmas.exists(lemma => lemma == "NNP" || lemma == "NNPS")

  def stripPostag(target: String, part: Seq[(String, String, String)]) = {
    part.filter { case (pos, tok, lem) => target != pos }
  }
  def stripPostag(target: Regex, part: Seq[(String, String, String)]) = {
    part.filter { case (pos, tok, lem) => !target.pattern.matcher(pos).matches}
  }
  def stripLemma(target: String, part: Seq[(String, String, String)]) = {
    part.filter { case (pos, tok, lem) => target != lem }
  }
  
  def cleanArg(part: Seq[(String, String, String)]) = stripPostag("DT", part)
  
  def zip3(l1 : List[String], l2 : List[String],l3 : List[String]) : List[(String, String, String)] =
  {
    def zip3$ (l1$ : List[String], l2$ : List[String], l3$ : List[String], acc : List[(String, String, String)]) : List[(String, String, String)] = l1$ match
    {
      case Nil => acc reverse
      case l1$head :: l1$tail => zip3$(l1$tail, l2$.tail, l3$.tail, (l1$head, l2$.head, l3$.head) :: acc)
    }

    zip3$(l1, l2, l3, List[(String,String,String)]())
  }
}
