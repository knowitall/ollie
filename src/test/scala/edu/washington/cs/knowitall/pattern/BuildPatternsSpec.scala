package edu.washington.cs.knowitall.pattern

import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.Specification

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer

@RunWith(classOf[JUnitSuiteRunner])
class BuildPatternsTest extends JUnit4(BuildPatternsSpec)
object BuildPatternsSpec extends Specification {
  def findPatterns(row: (String, String, String, String, String), maxLength: Option[Int] = None) = {
    val (rel, arg1, arg2, lemmasString, pickled) = row
    val lemmas = lemmasString.split("\\s+").toSet
    val graph = DependencyGraph.deserialize(pickled).map(_.lemmatize(MorphaStemmer.instance)).normalize
    BuildPatterns.findRelationPatterns(graph, rel, arg1, arg2, lemmas, maxLength)
  }

  "A pattern is found when the argument overlap" in {
    val row, (arg1, rel, arg2, lemmas, pickled) = ("be marry to", "hillary clinton", "bill clinton", "hillary clinton marry bill", "cc(married_VBN_11_0, And_CC_0_0); nn(Clinton_NNP_2_0, Hillary_NNP_1_0); nsubjpass(married_VBN_11_0, Clinton_NNP_2_0); punct(Clinton_NNP_2_0, _,_3_0); dep(know_VBP_8_0, who_WP_4_0); punct(know_VBP_8_0, _,_5_0); mark(know_VBP_8_0, as_IN_6_0); nsubj(know_VBP_8_0, we_PRP_7_0); rcmod(Clinton_NNP_2_0, know_VBP_8_0); punct(Clinton_NNP_2_0, _,_9_0); auxpass(married_VBN_11_0, is_VBZ_10_0); nn(Clinton_NNP_14_0, Bill_NNP_13_0); prep_to(married_VBN_11_0, Clinton_NNP_14_0); punct(married_VBN_11_0, ._._15_0)")
    val patterns = findPatterns(row)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubjpass< {rel:postag=VBN} >prep_to> {arg2}"
  }

  "A pattern is found with exactly one slot" in {
    val row = ("arrive in", "barack obama", "afghanistan", "barack obama arrive afghanistan", "(to_TO_4_0), (in_IN_12_0), (on_IN_14_0), (or_CC_16_0), (for_IN_20_0), (to_TO_23_0), (and_CC_27_0), (in_IN_29_0), (of_IN_34_0), (from_IN_38_0), poss(trip_NN_3_0, his_PRP$_1_0); amod(trip_NN_3_0, two-day_JJ_2_0); pobj(After_IN_0_0, trip_NN_3_0); prep_to(trip_NN_3_0, Afghanistan_NNP_5_0); punct(trip_NN_3_0, ,_,_6_0); nn(Obama_NNP_10_0, U.S._NNP_7_0); nn(Obama_NNP_10_0, Senator_NNP_8_0); nn(Obama_NNP_10_0, Barack_NNP_9_0); nsubj(arrived_VBD_11_0, Obama_NNP_10_0); rcmod(trip_NN_3_0, arrived_VBD_11_0); prep_in(arrived_VBD_11_0, Iraq_NNP_13_0); prep_on(arrived_VBD_11_0, Monday_NNP_15_0); prep_on(arrived_VBD_11_0, July_NNP_17_0); conj_or(Monday_NNP_15_0, July_NNP_17_0); num(July_NNP_17_0, 21_CD_18_0); punct(trip_NN_3_0, ,_,_19_0); det(visit_NN_22_0, a_DT_21_0); prep_for(trip_NN_3_0, visit_NN_22_0); det(East_NNP_26_0, the_DT_24_0); nn(East_NNP_26_0, Middle_NNP_25_0); prep_to(visit_NN_22_0, East_NNP_26_0); prep_to(visit_NN_22_0, Europe_NNP_28_0); conj_and(East_NNP_26_0, Europe_NNP_28_0); poss(capacity_NN_31_0, his_PRP$_30_0); prep_in(visit_NN_22_0, capacity_NN_31_0); det(member_NN_33_0, a_DT_32_0); dep(capacity_NN_31_0, member_NN_33_0); det(Senate_NNP_37_0, the_DT_35_0); nn(Senate_NNP_37_0, U.S._NNP_36_0); prep_of(member_NN_33_0, Senate_NNP_37_0); prep_from(member_NN_33_0, Illinois_NNP_39_0); punct(After_IN_0_0, ._._40_0)")
    val patterns = findPatterns(row)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubj< {rel:postag=VBD} <rcmod< {slot0:postag=NN} >prep_to> {arg2}"
  }

  "A pattern is NOT found because of a length restriction" in {
    val row = ("arrive in", "barack obama", "afghanistan", "barack obama arrive afghanistan", "(to_TO_4_0), (in_IN_12_0), (on_IN_14_0), (or_CC_16_0), (for_IN_20_0), (to_TO_23_0), (and_CC_27_0), (in_IN_29_0), (of_IN_34_0), (from_IN_38_0), poss(trip_NN_3_0, his_PRP$_1_0); amod(trip_NN_3_0, two-day_JJ_2_0); pobj(After_IN_0_0, trip_NN_3_0); prep_to(trip_NN_3_0, Afghanistan_NNP_5_0); punct(trip_NN_3_0, ,_,_6_0); nn(Obama_NNP_10_0, U.S._NNP_7_0); nn(Obama_NNP_10_0, Senator_NNP_8_0); nn(Obama_NNP_10_0, Barack_NNP_9_0); nsubj(arrived_VBD_11_0, Obama_NNP_10_0); rcmod(trip_NN_3_0, arrived_VBD_11_0); prep_in(arrived_VBD_11_0, Iraq_NNP_13_0); prep_on(arrived_VBD_11_0, Monday_NNP_15_0); prep_on(arrived_VBD_11_0, July_NNP_17_0); conj_or(Monday_NNP_15_0, July_NNP_17_0); num(July_NNP_17_0, 21_CD_18_0); punct(trip_NN_3_0, ,_,_19_0); det(visit_NN_22_0, a_DT_21_0); prep_for(trip_NN_3_0, visit_NN_22_0); det(East_NNP_26_0, the_DT_24_0); nn(East_NNP_26_0, Middle_NNP_25_0); prep_to(visit_NN_22_0, East_NNP_26_0); prep_to(visit_NN_22_0, Europe_NNP_28_0); conj_and(East_NNP_26_0, Europe_NNP_28_0); poss(capacity_NN_31_0, his_PRP$_30_0); prep_in(visit_NN_22_0, capacity_NN_31_0); det(member_NN_33_0, a_DT_32_0); dep(capacity_NN_31_0, member_NN_33_0); det(Senate_NNP_37_0, the_DT_35_0); nn(Senate_NNP_37_0, U.S._NNP_36_0); prep_of(member_NN_33_0, Senate_NNP_37_0); prep_from(member_NN_33_0, Illinois_NNP_39_0); punct(After_IN_0_0, ._._40_0)")
    val patterns = findPatterns(row, Some(2))
    patterns.size must_== 0
  }

  // rel rel
  "A pattern is found" in {
    val row = ("be bear a", "queequag", "slave", "bear queequag slave", "(in_IN_5_0), (._._7_0), nsubjpass(born_VBN_2_0, Queequag_NNP_0_0); auxpass(born_VBN_2_0, was_VBD_1_0); dobj(born_VBN_2_0, slave_NN_4_0); det(slave_NN_4_0, a_DT_3_0); prep_in(slave_NN_4_0, Africa_NNP_6_0)")
    val patterns = findPatterns(row, Some(2))
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubjpass< {rel:postag=VBN} >dobj> {arg2}"
  }

  "A single pattern is found with a slot instead of a rel rel" in {
    val row = ("be elect president of", "barack obama", "unite state", "barack obama unite state elect president", "(of_IN_5_0), (._._9_0), nn(Obama_NNP_1_0, Barack_NNP_0_0); nsubjpass(elected_VBN_3_0, Obama_NNP_1_0); auxpass(elected_VBN_3_0, was_VBD_2_0); dobj(elected_VBN_3_0, president_NN_4_0); prep_of(president_NN_4_0, States_NNPS_8_0); det(States_NNPS_8_0, the_DT_6_0); nn(States_NNPS_8_0, United_NNP_7_0)")
    val patterns = findPatterns(row)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubjpass< {rel0:postag=VBN} >dobj> {rel1:postag=NN} >prep_of> {arg2}"
  }

  "A single pattern is found with a slot instead of a rel rel" in {
    val row = ("be team locate in", "mariner", "seattle", "mariner team locate seattle", "(in_IN_6_0), (._._8_0), det(Mariners_NNPS_1_0, The_DT_0_0); nsubj(team_NN_4_0, Mariners_NNPS_1_0); cop(team_NN_4_0, are_VBP_2_0); det(team_NN_4_0, a_DT_3_0); partmod(team_NN_4_0, located_VBN_5_0); prep_in(located_VBN_5_0, Seattle_NNP_7_0)")
    val patterns = findPatterns(row)
    patterns.head._1.toString must_== "{arg1} <nsubj< {rel0:postag=NN} >partmod> {rel1:postag=VBN} >prep_in> {arg2}"
  }

  "A single pattern is found with a slot instead of a rel rel" in {
    val row = ("be going populate", "human", "earth", "human go populate earth", "(._._7_0), nsubj(going_VBG_2_0, Humans_NNS_0_0); aux(going_VBG_2_0, are_VBP_1_0); xcomp(going_VBG_2_0, populate_VB_4_0); aux(populate_VB_4_0, to_TO_3_0); dobj(populate_VB_4_0, earth_NN_6_0); det(earth_NN_6_0, the_DT_5_0)")
    val patterns = findPatterns(row)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubj< {slot0:postag=VBG} >xcomp> {rel:postag=VB} >dobj> {arg2}"
  }

  "A single pattern is found with a slot instead of a rel rel" in {
    val row = ("have crush on", "juliette", "romeo", "juliette have crush romeo", "(on_IN_4_0), (._._6_0), nsubj(has_VBZ_1_0, Juliette_NNP_0_0); dobj(has_VBZ_1_0, crush_NN_3_0); det(crush_NN_3_0, a_DT_2_0); prep_on(crush_NN_3_0, Romeo_NNP_5_0)")
    val patterns = findPatterns(row)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubj< {rel0:postag=VBZ} >dobj> {rel1:postag=NN} >prep_on> {arg2}"
  }
}
