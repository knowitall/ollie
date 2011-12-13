package edu.washington.cs.knowitall
package pattern

import org.junit._
import org.junit.Assert._
import org.specs.Specification
import org.specs.runner.JUnit4
import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import tool.parse.graph.DependencyGraph
import tool.parse.graph.Dependencies
import tool.stem.MorphaStemmer

@RunWith(classOf[JUnitSuiteRunner])
class TreePatternLearnerTest extends JUnit4(TreePatternLearnerSpec)
object TreePatternLearnerSpec extends Specification {
  val row, (arg1, rel, arg2, lemmas, deps) = ("hillary clinton", "be marry to", "bill clinton", Set("hillary", "clinton", "marry", "bill"), Dependencies.deserialize("cc(married_VBN_11, And_CC_0); nn(Clinton_NNP_2, Hillary_NNP_1); nsubjpass(married_VBN_11, Clinton_NNP_2); punct(Clinton_NNP_2, _,_3); dep(know_VBP_8, who_WP_4); punct(know_VBP_8, _,_5); mark(know_VBP_8, as_IN_6); nsubj(know_VBP_8, we_PRP_7); rcmod(Clinton_NNP_2, know_VBP_8); punct(Clinton_NNP_2, _,_9); auxpass(married_VBN_11, is_VBZ_10); nn(Clinton_NNP_14, Bill_NNP_13); prep_to(married_VBN_11, Clinton_NNP_14); punct(married_VBN_11, ._._15)"))
    
  "a pattern is found" in {
    val graph = DependencyGraph(deps.map(_.lemmatize(MorphaStemmer.instance))).normalize
    val patterns = TreePatternLearner.findPatternsForLDA(graph, lemmas, Map(arg1 -> "arg1", arg2 -> "arg2"), rel, None)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubjpass< {rel:VBN} >prep_to> {arg2}"
  }
}
