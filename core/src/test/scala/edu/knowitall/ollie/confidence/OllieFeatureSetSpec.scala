package edu.knowitall.ollie.confidence

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.ollie.Ollie
import edu.knowitall.ollie.OllieExtractionInstance
import edu.knowitall.ollie.ScoredOllieExtractionInstance
import edu.knowitall.openparse.OpenParse
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object OllieFeatureSetSpec extends Specification {
  val ollie = new Ollie(OpenParse.withDefaultModel())

  "if right before arg1" in {
    val graph = DependencyGraph.deserialize("poss(father_NN_2_12, his_PRP$_1_8); punct(father_NN_2_12, ,_,_3_19); appos(father_NN_2_12, Whitechapel_NNP_4_21); punct(father_NN_2_12, ,_,_5_33); advmod(betrays_VBZ_6_35, However_RB_0_0); nsubj(betrays_VBZ_6_35, father_NN_2_12); dobj(betrays_VBZ_6_35, whereabouts_NN_8_47); punct(betrays_VBZ_6_35, ,_,_9_59); xcomp(betrays_VBZ_6_35, fearing_VBG_10_61); punct(betrays_VBZ_6_35, ._._27_149); poss(whereabouts_NN_8_47, his_PRP$_7_43); ccomp(fearing_VBG_10_61, die_VB_15_87); poss(son_NN_13_78, his_PRP$_12_74); complm(die_VB_15_87, that_IN_11_69); nsubj(die_VB_15_87, son_NN_13_78); aux(die_VB_15_87, will_MD_14_82); advcl(die_VB_15_87, captured_VBN_20_104); mark(captured_VBN_20_104, if_IN_16_91); nsubjpass(captured_VBN_20_104, he_PRP_17_94); auxpass(captured_VBN_20_104, is_VBZ_18_97); neg(captured_VBN_20_104, not_RB_19_100); cc(captured_VBN_20_104, and_CC_21_113); conj(captured_VBN_20_104, returned_VBN_22_117); dobj(captured_VBN_20_104, home_NN_23_126); prep(captured_VBN_20_104, to_TO_24_131); pobj(to_TO_24_131, plantation_NN_26_138); det(plantation_NN_26_138, the_DT_25_134)")
    val extrs = ollie.extract(graph)

    val extr = extrs.toSeq(2)
    OllieFeatures.ifRightBeforeArg1(extr) must_== 1.0
  }
}
