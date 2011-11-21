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
import tool.parse.pattern.DependencyPattern

@RunWith(classOf[JUnitSuiteRunner])
class PatternExtractorSpecTest extends JUnit4(PatternExtractorSpec)
object PatternExtractorSpec extends Specification {
  def testAvoidNeg {
    val sentence = "I 'm not a huge fan of John M. but these recordings do sound merakedly better than an mp3 or AAC."
    val deps = Dependencies.deserialize("nsubj(fan_NN_5, I_PRP_0); cop(fan_NN_5, 'm_VBP_1); neg(fan_NN_5, not_RB_2); det(fan_NN_5, a_DT_3); amod(fan_NN_5, huge_JJ_4); nn(M._NNP_8, John_NNP_7); prep_of(fan_NN_5, M._NNP_8); det(recordings_NNS_11, these_DT_10); nsubj(sound_VB_13, recordings_NNS_11); aux(sound_VB_13, do_VBP_12); conj_but(fan_NN_5, sound_VB_13); advmod(better_JJR_15, merakedly_RB_14); acomp(sound_VB_13, better_JJR_15); det(mp3_NN_18, an_DT_17); prep_than(better_JJR_15, mp3_NN_18); prep_than(better_JJR_15, AAC._NN_20); conj_or(mp3_NN_18, AAC._NN_20)")
    val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {arg2} >cop> {rel}")

    "no pattern is found because of a neg edge" in {
      val graph = new DependencyGraph(deps.map(_.lemmatize(MorphaStemmer.instance))).collapseNounGroups.collapseNNPOf
      val extractions = PatternExtractor.extract(graph, pattern)
      extractions must be empty
    }
  }
  
  testAvoidNeg
}