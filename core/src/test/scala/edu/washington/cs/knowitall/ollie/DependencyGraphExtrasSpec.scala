package edu.washington.cs.knowitall.ollie

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

@RunWith(classOf[JUnitRunner])
object DependencyGraphExtrasTest extends Specification {
  "switch to passive voice works" in {
    val graph = DependencyGraph.deserialize("nsubj(hit_VBD_1_8, Michael_NNP_0_0); dobj(hit_VBD_1_8, ball_NN_3_16); punct(hit_VBD_1_8, ._._4_20); det(ball_NN_3_16, the_DT_2_12)")
    val extras = new DependencyGraphExtras(graph)

    val switched = extras.switchVoice

    switched.size must_== 1
    switched.head.serialize must_== "det(ball_NN_1_4, the_DT_0_0); auxpass(hit_VBD_2_13, was_VBD_1_9); nsubjpass(hit_VBD_2_13, ball_NN_1_4); prep(hit_VBD_2_13, by_IN_3_17); punct(hit_VBD_2_13, ._._6_28); pobj(by_IN_3_17, Michael_NNP_4_20)"
  }

  "switch to active voice works" in {
    val graph = DependencyGraph.deserialize("det(ball_NN_1_4, The_DT_0_0); nsubjpass(hit_VBN_3_13, ball_NN_1_4); auxpass(hit_VBN_3_13, was_VBD_2_9); prep(hit_VBN_3_13, by_IN_4_17); punct(hit_VBN_3_13, ._._6_27); pobj(by_IN_4_17, Michael_NNP_5_20)")
    val extras = new DependencyGraphExtras(graph)

    val switched = extras.switchVoice

    switched.size must_== 1
    switched.head.serialize must_== "nsubj(hit_VBN_1_8, Michael_NNP_0_0); dobj(hit_VBN_1_8, ball_NN_3_16); punct(hit_VBN_1_8, ._._4_21); det(ball_NN_3_16, The_DT_2_12)"
  }
}
