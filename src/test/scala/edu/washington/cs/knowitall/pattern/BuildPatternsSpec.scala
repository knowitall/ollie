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
class BuildPatternsTest extends JUnit4(BuildPatternsSpec)
object BuildPatternsSpec extends Specification {
  def findPatterns(row: (String, String, String, String, String), maxLength: Option[Int] = None) = {
    val (rel, arg1, arg2, lemmasString, pickled) = row
    val lemmas = lemmasString.split("\\s+").toSet
    val graph = DependencyGraph.deserialize(pickled).map(_.lemmatize(MorphaStemmer.instance)).normalize
    BuildPatterns.findRelationPatterns(graph, rel, arg1, arg2, lemmas, maxLength)
  }
  
  "A pattern is found when the argument overlap" in {
    val row, (arg1, rel, arg2, lemmas, pickled) = ("be marry to", "hillary clinton", "bill clinton", "hillary clinton marry bill", "cc(married_VBN_11, And_CC_0); nn(Clinton_NNP_2, Hillary_NNP_1); nsubjpass(married_VBN_11, Clinton_NNP_2); punct(Clinton_NNP_2, _,_3); dep(know_VBP_8, who_WP_4); punct(know_VBP_8, _,_5); mark(know_VBP_8, as_IN_6); nsubj(know_VBP_8, we_PRP_7); rcmod(Clinton_NNP_2, know_VBP_8); punct(Clinton_NNP_2, _,_9); auxpass(married_VBN_11, is_VBZ_10); nn(Clinton_NNP_14, Bill_NNP_13); prep_to(married_VBN_11, Clinton_NNP_14); punct(married_VBN_11, ._._15)")
    val patterns = findPatterns(row)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubjpass< {rel:postag=VBN} >prep_to> {arg2}"
  }

  "A pattern is found with exactly one slot" in {
    val row = ("arrive in", "barack obama", "afghanistan", "barack obama arrive afghanistan", "(to_TO_4), (in_IN_12), (on_IN_14), (or_CC_16), (for_IN_20), (to_TO_23), (and_CC_27), (in_IN_29), (of_IN_34), (from_IN_38), poss(trip_NN_3, his_PRP$_1); amod(trip_NN_3, two-day_JJ_2); pobj(After_IN_0, trip_NN_3); prep_to(trip_NN_3, Afghanistan_NNP_5); punct(trip_NN_3, ,_,_6); nn(Obama_NNP_10, U.S._NNP_7); nn(Obama_NNP_10, Senator_NNP_8); nn(Obama_NNP_10, Barack_NNP_9); nsubj(arrived_VBD_11, Obama_NNP_10); rcmod(trip_NN_3, arrived_VBD_11); prep_in(arrived_VBD_11, Iraq_NNP_13); prep_on(arrived_VBD_11, Monday_NNP_15); prep_on(arrived_VBD_11, July_NNP_17); conj_or(Monday_NNP_15, July_NNP_17); num(July_NNP_17, 21_CD_18); punct(trip_NN_3, ,_,_19); det(visit_NN_22, a_DT_21); prep_for(trip_NN_3, visit_NN_22); det(East_NNP_26, the_DT_24); nn(East_NNP_26, Middle_NNP_25); prep_to(visit_NN_22, East_NNP_26); prep_to(visit_NN_22, Europe_NNP_28); conj_and(East_NNP_26, Europe_NNP_28); poss(capacity_NN_31, his_PRP$_30); prep_in(visit_NN_22, capacity_NN_31); det(member_NN_33, a_DT_32); dep(capacity_NN_31, member_NN_33); det(Senate_NNP_37, the_DT_35); nn(Senate_NNP_37, U.S._NNP_36); prep_of(member_NN_33, Senate_NNP_37); prep_from(member_NN_33, Illinois_NNP_39); punct(After_IN_0, ._._40)")
    val patterns = findPatterns(row)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubj< {rel:postag=VBD} <rcmod< {slot0:postag=NN} >prep_to> {arg2}"
  }
  
  "A pattern is NOT found because of a length restriction" in {
    val row = ("arrive in", "barack obama", "afghanistan", "barack obama arrive afghanistan", "(to_TO_4), (in_IN_12), (on_IN_14), (or_CC_16), (for_IN_20), (to_TO_23), (and_CC_27), (in_IN_29), (of_IN_34), (from_IN_38), poss(trip_NN_3, his_PRP$_1); amod(trip_NN_3, two-day_JJ_2); pobj(After_IN_0, trip_NN_3); prep_to(trip_NN_3, Afghanistan_NNP_5); punct(trip_NN_3, ,_,_6); nn(Obama_NNP_10, U.S._NNP_7); nn(Obama_NNP_10, Senator_NNP_8); nn(Obama_NNP_10, Barack_NNP_9); nsubj(arrived_VBD_11, Obama_NNP_10); rcmod(trip_NN_3, arrived_VBD_11); prep_in(arrived_VBD_11, Iraq_NNP_13); prep_on(arrived_VBD_11, Monday_NNP_15); prep_on(arrived_VBD_11, July_NNP_17); conj_or(Monday_NNP_15, July_NNP_17); num(July_NNP_17, 21_CD_18); punct(trip_NN_3, ,_,_19); det(visit_NN_22, a_DT_21); prep_for(trip_NN_3, visit_NN_22); det(East_NNP_26, the_DT_24); nn(East_NNP_26, Middle_NNP_25); prep_to(visit_NN_22, East_NNP_26); prep_to(visit_NN_22, Europe_NNP_28); conj_and(East_NNP_26, Europe_NNP_28); poss(capacity_NN_31, his_PRP$_30); prep_in(visit_NN_22, capacity_NN_31); det(member_NN_33, a_DT_32); dep(capacity_NN_31, member_NN_33); det(Senate_NNP_37, the_DT_35); nn(Senate_NNP_37, U.S._NNP_36); prep_of(member_NN_33, Senate_NNP_37); prep_from(member_NN_33, Illinois_NNP_39); punct(After_IN_0, ._._40)")
    val patterns = findPatterns(row, Some(2))
    patterns.size must_== 0
  }
  
  // rel rel
  "A pattern is found" in {
    val row = ("be bear a", "queequag", "slave", "bear queequag slave", "(in_IN_5), (._._7), nsubjpass(born_VBN_2, Queequag_NNP_0); auxpass(born_VBN_2, was_VBD_1); dobj(born_VBN_2, slave_NN_4); det(slave_NN_4, a_DT_3); prep_in(slave_NN_4, Africa_NNP_6)")
    val patterns = findPatterns(row, Some(2))
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubjpass< {rel:postag=VBN} >dobj> {arg2}"
  }
  
  "A single pattern is found with a slot instead of a rel rel" in {
    val row = ("be elect president of", "barack obama", "unite state", "barack obama unite state elect president", "(of_IN_5), (._._9), nn(Obama_NNP_1, Barack_NNP_0); nsubjpass(elected_VBN_3, Obama_NNP_1); auxpass(elected_VBN_3, was_VBD_2); dobj(elected_VBN_3, president_NN_4); prep_of(president_NN_4, States_NNPS_8); det(States_NNPS_8, the_DT_6); nn(States_NNPS_8, United_NNP_7)")
    val patterns = findPatterns(row)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubjpass< {rel0:postag=VBN} >dobj> {rel1:postag=NN} >prep_of> {arg2}"
  }
  
  "A single pattern is found with a slot instead of a rel rel" in {
    val row = ("be team locate in", "mariner", "seattle", "mariner team locate seattle", "(in_IN_6), (._._8), det(Mariners_NNPS_1, The_DT_0); nsubj(team_NN_4, Mariners_NNPS_1); cop(team_NN_4, are_VBP_2); det(team_NN_4, a_DT_3); partmod(team_NN_4, located_VBN_5); prep_in(located_VBN_5, Seattle_NNP_7)")
    val patterns = findPatterns(row)
    patterns.head._1.toString must_== "{arg1} <nsubj< {rel0:postag=NN} >partmod> {rel1:postag=VBN} >prep_in> {arg2}"
  }
  
  "A single pattern is found with a slot instead of a rel rel" in {
    val row = ("be going populate", "human", "earth", "human go populate earth", "(._._7), nsubj(going_VBG_2, Humans_NNS_0); aux(going_VBG_2, are_VBP_1); xcomp(going_VBG_2, populate_VB_4); aux(populate_VB_4, to_TO_3); dobj(populate_VB_4, earth_NN_6); det(earth_NN_6, the_DT_5)")
    val patterns = findPatterns(row)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubj< {slot0:postag=VBG} >xcomp> {rel:postag=VB} >dobj> {arg2}"
  }
  
  "A single pattern is found with a slot instead of a rel rel" in {
    val row = ("have crush on", "juliette", "romeo", "juliette have crush romeo", "(on_IN_4), (._._6), nsubj(has_VBZ_1, Juliette_NNP_0); dobj(has_VBZ_1, crush_NN_3); det(crush_NN_3, a_DT_2); prep_on(crush_NN_3, Romeo_NNP_5)")
    val patterns = findPatterns(row)
    patterns.size must_== 1
    patterns.head._1.toString must_== "{arg1} <nsubj< {rel0:postag=VBZ} >dobj> {rel1:postag=NN} >prep_on> {arg2}"
  }
}
