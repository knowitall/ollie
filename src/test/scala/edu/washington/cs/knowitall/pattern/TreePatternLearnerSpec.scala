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
import tool.parse.pattern.CaptureNodeMatcher

@RunWith(classOf[JUnitSuiteRunner])
class TreePatternLearnerTest extends JUnit4(TreePatternLearnerSpec)
object TreePatternLearnerSpec extends Specification {
    
  "a pattern is found" in {
    val row, (arg1, rel, arg2, lemmas, pickled) = ("hillary clinton", "be marry to", "bill clinton", Set("hillary", "clinton", "marry", "bill"), "cc(married_VBN_11, And_CC_0); nn(Clinton_NNP_2, Hillary_NNP_1); nsubjpass(married_VBN_11, Clinton_NNP_2); punct(Clinton_NNP_2, _,_3); dep(know_VBP_8, who_WP_4); punct(know_VBP_8, _,_5); mark(know_VBP_8, as_IN_6); nsubj(know_VBP_8, we_PRP_7); rcmod(Clinton_NNP_2, know_VBP_8); punct(Clinton_NNP_2, _,_9); auxpass(married_VBN_11, is_VBZ_10); nn(Clinton_NNP_14, Bill_NNP_13); prep_to(married_VBN_11, Clinton_NNP_14); punct(married_VBN_11, ._._15)")
    val graph = DependencyGraph.deserialize(pickled).map(_.lemmatize(MorphaStemmer.instance)).normalize
    val patterns = TreePatternLearner.findPatternsForLDA(graph, lemmas, Map(arg1 -> "arg1", arg2 -> "arg2"), rel, None)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubjpass< {rel:postag=VBN} >prep_to> {arg2}"
  }
    
  "no pattern is found" in {
    val row, (rel, arg1, arg2, lemmas, pickled) = ("live in", "carol", "new york", Set("carol", "live", "new", "york"), "advmod(WAS_VBD_2, WHEN_WRB_0); nsubj(WAS_VBD_2, CHRISTOPHER_NNP_1); dobj(WAS_VBD_2, BORN_NNP_3); punct(BORN_NNP_3, _,_4); dobj(WAS_VBD_2, KEN_NNP_5); conj_and(BORN_NNP_3, KEN_NNP_5); nn(LIVED_NNP_8, CAROL_NNP_7); dobj(WAS_VBD_2, LIVED_NNP_8); conj_and(BORN_NNP_3, LIVED_NNP_8); nn(YORK._NNP_11, NEW_NNP_10); prep_in(WAS_VBD_2, YORK._NNP_11)")
    val graph = DependencyGraph.deserialize(pickled).map(_.lemmatize(MorphaStemmer.instance)).normalize
    TreePatternLearner.findPatternsForLDA(graph, lemmas, Map(arg1 -> "arg1", arg2 -> "arg2"), rel, None) must throwAn[TreePatternLearner.NoRelationNodeException]
  }
}
