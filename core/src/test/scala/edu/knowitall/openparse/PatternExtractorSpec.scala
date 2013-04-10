package edu.knowitall.openparse

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import edu.knowitall.ollie.Ollie.stemmer
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.parse.graph.Dependencies
import edu.knowitall.tool.parse.graph.DependencyPattern

import OpenParse._
import extract._

@RunWith(classOf[JUnitRunner])
object PatternExtractorSpecTest extends Specification {
  def testPostagConstraint {
    val sentence = "Angels appear in the Bible story from the first pages of Genesis right through to the final pages of the Book of Revelation ."
    val graph = DependencyGraph.deserialize("nsubj(appear_VB_1_0, Angels_NNPS_0_0); det(story_NN_5_0, the_DT_3_0); nn(story_NN_5_0, Bible_NNP_4_0); prep_in(appear_VB_1_0, story_NN_5_0); det(pages_NNS_9_0, the_DT_7_0); amod(pages_NNS_9_0, first_JJ_8_0); prep_from(appear_VB_1_0, pages_NNS_9_0); nn(right_NN_12_0, Genesis_NNP_11_0); prep_of(pages_NNS_9_0, right_NN_12_0); dep(appear_VB_1_0, through_IN_13_0); dep(through_IN_13_0, to_TO_14_0); det(pages_NNS_17_0, the_DT_15_0); amod(pages_NNS_17_0, final_JJ_16_0); pobj(to_TO_14_0, pages_NNS_17_0); det(Book_NNP_20_0, the_DT_19_0); prep_of(pages_NNS_17_0, Book_NNP_20_0); prep_of(Book_NNP_20_0, Revelation_NNP_22_0); punct(appear_VB_1_0, ._._23_0)").
        normalize

    "(Angels, appear, the Bible) is found without a postag constraint" in {
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel} >prep_in> {arg2}")
      val extractions = new GeneralExtractor(pattern, 1.0).extract(graph)
      extractions.size must_== 1
      extractions.map(_.toString) must contain("(Angels; appear; the Bible story)")
    }

    "(Angels, appear, the Bible) is found with a postag constraint" in {
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel:postag=VB} >prep_in> {arg2}")
      val extractions = new GeneralExtractor(pattern, 1.0).extract(graph)
      extractions.size must_== 1
      extractions.map(_.toString) must contain("(Angels; appear; the Bible story)")
    }

    "(Angels, appear, the Bible) is not found with the wrong postag constraint" in {
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel:postag=XXX} >prep_in> {arg2}")
      val extractions = new GeneralExtractor(pattern, 1.0).extract(graph)
      extractions.size must_== 0
    }
  }

  def testRelnounCases {
    "adjective descriptor relnoun" in {
      val sentence = "US President Barack Obama went to the grocery store."
      val pickled = "nn(Obama_NNP_3_0, US_NNP_0_0); nn(Obama_NNP_3_0, President_NNP_1_0); nn(Obama_NNP_3_0, Barack_NNP_2_0); nsubj(went_VBD_4_0, Obama_NNP_3_0); prep(went_VBD_4_0, to_TO_5_0); det(store_NN_8_0, the_DT_6_0); nn(store_NN_8_0, grocery_NN_7_0); pobj(to_TO_5_0, store_NN_8_0)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{rel} <nn< {arg1} >nn> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; US)"))
    }

    "adjective descriptor appositive relnoun" in {
      val sentence = "Barack Obama, the US President, went to the store."
      val pickled = "nn(Obama_NNP_1_0, Barack_NNP_0_0); nsubj(went_VBD_7_0, Obama_NNP_1_0); det(President_NNP_5_0, the_DT_3_0); nn(President_NNP_5_0, US_NNP_4_0); appos(Obama_NNP_1_0, President_NNP_5_0); det(store_NN_10_0, the_DT_9_0); prep_to(went_VBD_7_0, store_NN_10_0)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel} >nn> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; US)"))
    }

    "appositive relnoun" in {
      val sentence = "Barack Obama, the president of the US, went to the grocery store."
      val pickled = "nn(Obama_NNP_1_0, Barack_NNP_0_0); nsubj(went_VBD_9_0, Obama_NNP_1_0); det(president_NN_4_0, the_DT_3_0); appos(Obama_NNP_1_0, president_NN_4_0); det(US_NNP_7_0, the_DT_6_0); prep_of(president_NN_4_0, US_NNP_7_0); det(store_NN_13_0, the_DT_11_0); nn(store_NN_13_0, grocery_NN_12_0); prep_to(went_VBD_9_0, store_NN_13_0)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel} >prep_of> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; the US)"))
    }

    "possessive relnoun" in {
      val sentence = "Barack Obama, the president of the US, went to the grocery store."
      val pickled = "nn(Obama_NNP_1_0, Barack_NNP_0_0); nsubj(went_VBD_9_0, Obama_NNP_1_0); det(president_NN_4_0, the_DT_3_0); appos(Obama_NNP_1_0, president_NN_4_0); det(US_NNP_7_0, the_DT_6_0); prep_of(president_NN_4_0, US_NNP_7_0); det(store_NN_13_0, the_DT_11_0); nn(store_NN_13_0, grocery_NN_12_0); prep_to(went_VBD_9_0, store_NN_13_0)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel} >prep_of> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; the US)"))
    }

    "possessive appositive relnoun" in {
      val sentence = "The US's president, Barack Obama, went to the store."
      val pickled = "det(US_NNP_1_0, The_DT_0_0); poss(president_NN_3_0, US_NNP_1_0); nsubj(went_VBD_8_0, president_NN_3_0); nn(Obama_NNP_6_0, Barack_NNP_5_0); appos(president_NN_3_0, Obama_NNP_6_0); det(store_NN_11_0, the_DT_10_0); prep_to(went_VBD_8_0, store_NN_11_0)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} <appos< {rel} >poss> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; The US)"))
    }

    "reverse possessive appositive relnoun" in {
      val sentence = "Barack Obama, the US's president, went to the store."
      val pickled = "nn(Obama_NNP_1_0, Barack_NNP_0_0); nsubj(went_VBD_8_0, Obama_NNP_1_0); det(US_NNP_4_0, the_DT_3_0); poss(president_NN_6_0, US_NNP_4_0); appos(Obama_NNP_1_0, president_NN_6_0); det(store_NN_11_0, the_DT_10_0); prep_to(went_VBD_8_0, store_NN_11_0)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel} >poss> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; the US)"))
    }

    "possessive is relnoun" in {
      val sentence = "The US's President is Barack Obama."
      val pickled = "det(US_NNP_1_0, The_DT_0_0); poss(President_NNP_3_0, US_NNP_1_0); nsubj(Obama_NNP_6_0, President_NNP_3_0); cop(Obama_NNP_6_0, is_VBZ_4_0); nn(Obama_NNP_6_0, Barack_NNP_5_0)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >nsubj> {rel} >poss> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; The US)"))
    }

    "is possessive relnoun" in {
      val sentence = "Barack Obama is the US's president."
      val pickled = "nn(Obama_NNP_1_0, Barack_NNP_0_0); nsubj(president_NN_6_0, Obama_NNP_1_0); cop(president_NN_6_0, is_VBZ_2_0); det(US_NNP_4_0, the_DT_3_0); poss(president_NN_6_0, US_NNP_4_0)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel} >poss> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; the US)"))
    }

    "of is relnoun" in {
      val sentence = "The president of the US is Barack Obama."
      val pickled = "det(president_NN_1_0, The_DT_0_0); nsubj(Obama_NNP_7_0, president_NN_1_0); det(US_NNP_4_0, the_DT_3_0); prep_of(president_NN_1_0, US_NNP_4_0); cop(Obama_NNP_7_0, is_VBZ_5_0); nn(Obama_NNP_7_0, Barack_NNP_6_0)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >nsubj> {rel} >prep_of> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; the US)"))
    }
  }

  def testRelRel = {
    "A rel rel pattern is found" in {
      val pickled = "(of_IN_5_0), (._._9_0), nn(Obama_NNP_1_0, Barack_NNP_0_0); nsubjpass(elected_VBN_3_0, Obama_NNP_1_0); auxpass(elected_VBN_3_0, was_VBD_2_0); dobj(elected_VBN_3_0, president_NN_4_0); prep_of(president_NN_4_0, States_NNPS_8_0); det(States_NNPS_8_0, the_DT_6_0); nn(States_NNPS_8_0, United_NNP_7_0)"
      val graph = DependencyGraph.deserialize(pickled)
      val pattern = DependencyPattern.deserialize("{arg1} <nsubjpass< {rel1} >dobj> {rel2} >prep_of> {arg2}")
      val extractor = new TemplateExtractor(Template.deserialize("be {rel} of"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; was elected president of; the United States)"))
    }

    "A rel rel pattern is found" in {
      val pickled = "(in_IN_6_0), (._._8_0), det(Mariners_NNPS_1_0, The_DT_0_0); nsubj(team_NN_4_0, Mariners_NNPS_1_0); cop(team_NN_4_0, are_VBP_2_0); det(team_NN_4_0, a_DT_3_0); partmod(team_NN_4_0, located_VBN_5_0); prep_in(located_VBN_5_0, Seattle_NNP_7_0)"
      val graph = DependencyGraph.deserialize(pickled)
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel1} >partmod> {rel2} >prep_in> {arg2}")
      val extractor = new TemplateExtractor(Template.deserialize("be {rel} in"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(The Mariners; are a team located in; Seattle)"))
    }

    "A rel rel pattern is found" in {
      val pickled = "(._._7_0), nsubj(going_VBG_2_0, Humans_NNS_0_0); aux(going_VBG_2_0, are_VBP_1_0); xcomp(going_VBG_2_0, populate_VB_4_0); aux(populate_VB_4_0, to_TO_3_0); dobj(populate_VB_4_0, earth_NN_6_0); det(earth_NN_6_0, the_DT_5_0)"
      val graph = DependencyGraph.deserialize(pickled)
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel1} >xcomp> {rel2} >dobj> {arg2}")
      val extractor = new TemplateExtractor(Template.deserialize("be {rel}"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Humans; are going to populate; the earth)"))
    }

    "A rel rel pattern is found" in {
      val pickled = "(on_IN_4_0), (._._6_0), nsubj(has_VBZ_1_0, Juliette_NNP_0_0); dobj(has_VBZ_1_0, crush_NN_3_0); det(crush_NN_3_0, a_DT_2_0); prep_on(crush_NN_3_0, Romeo_NNP_5_0)"
      val graph = DependencyGraph.deserialize(pickled)
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel1} >dobj> {rel2} >prep_on> {arg2}")
      val extractor = new TemplateExtractor(Template.deserialize("{rel} on"), pattern, 1.0)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Juliette; has a crush on; Romeo)"))
    }
  }

  "relation expands across 'chaotically'" in {
    val sentence = "The people fled away chaotically towards the barn."
    val pickled = "det(people_NNS_1_0, The_DT_0_0); nsubj(fled_VBD_2_0, people_NNS_1_0); advmod(fled_VBD_2_0, chaotically_RB_3_0); det(barn_NN_6_0, the_DT_5_0); prep_towards(fled_VBD_2_0, barn_NN_6_0)"
    val graph = DependencyGraph.deserialize(pickled) // don't normalize
    val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel} >prep_towards> {arg2}")
    val extractor = new GeneralExtractor(pattern, 1.0)
    extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(The people; fled chaotically; the barn)"))
  }

  "noun relation word expands across of when it doesn't overlap with the argument" in {
    val pickled = "(of_IN_4_0), (by_IN_8_0), (of_IN_15_0), (at_IN_17_0), (of_IN_20_0), (at_IN_22_0), (and_CC_25_0), nsubj(types_NNS_3_0, These_DT_0_0); cop(types_NNS_3_0, are_VBP_1_0); det(types_NNS_3_0, the_DT_2_0); prep_of(types_NNS_3_0, clues_NNS_5_0); punct(types_NNS_3_0, ,_,_24_0); conj_and(types_NNS_3_0, team_NN_27_0); punct(types_NNS_3_0, of..._._28_0); partmod(clues_NNS_5_0, ferreted_VBN_6_0); prt(ferreted_VBN_6_0, out_RP_7_0); agent(ferreted_VBN_6_0, Gosling_NNP_10_0); nn(Gosling_NNP_10_0, Sam_NNP_9_0); punct(Gosling_NNP_10_0, ,_,_11_0); appos(Gosling_NNP_10_0, professor_NN_14_0); det(professor_NN_14_0, an_DT_12_0); amod(professor_NN_14_0, associate_JJ_13_0); prep_of(professor_NN_14_0, psychology_NN_16_0); prep_at(professor_NN_14_0, University_NNP_19_0); det(University_NNP_19_0, the_DT_18_0); prep_of(University_NNP_19_0, Texas_NNP_21_0); prep_at(University_NNP_19_0, Austin_NNP_23_0); poss(team_NN_27_0, his_PRP$_26_0)"
    val graph = DependencyGraph.deserialize(pickled) // don't normalize
    val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel:postag=NN} >{prep:regex=prep_(.*)}> {arg2}")
    val extractor = new TemplateExtractor(Template.deserialize("be {rel} {prep}"), pattern, 1.0)
    extractor.extract(graph).map(_.toString) must contain("(Sam Gosling; be an associate professor of psychology at; the University of Texas)")
  }

  "noun relation word does NOT expands across of when it overlaps with the argument" in {
    val pickled = "(of_IN_4_0), (by_IN_8_0), (of_IN_15_0), (at_IN_17_0), (of_IN_20_0), (at_IN_22_0), (and_CC_25_0), nsubj(types_NNS_3_0, These_DT_0_0); cop(types_NNS_3_0, are_VBP_1_0); det(types_NNS_3_0, the_DT_2_0); prep_of(types_NNS_3_0, clues_NNS_5_0); punct(types_NNS_3_0, ,_,_24_0); conj_and(types_NNS_3_0, team_NN_27_0); punct(types_NNS_3_0, of..._._28_0); partmod(clues_NNS_5_0, ferreted_VBN_6_0); prt(ferreted_VBN_6_0, out_RP_7_0); agent(ferreted_VBN_6_0, Gosling_NNP_10_0); nn(Gosling_NNP_10_0, Sam_NNP_9_0); punct(Gosling_NNP_10_0, ,_,_11_0); appos(Gosling_NNP_10_0, professor_NN_14_0); det(professor_NN_14_0, an_DT_12_0); amod(professor_NN_14_0, associate_JJ_13_0); prep_of(professor_NN_14_0, psychology_NN_16_0); prep_at(professor_NN_14_0, University_NNP_19_0); det(University_NNP_19_0, the_DT_18_0); prep_of(University_NNP_19_0, Texas_NNP_21_0); prep_at(University_NNP_19_0, Austin_NNP_23_0); poss(team_NN_27_0, his_PRP$_26_0)"
    val graph = DependencyGraph.deserialize(pickled) // don't normalize
    val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel:postag=NN} >{prep:regex=prep_(.*)}> {arg2}")
    val extractor = new TemplateExtractor(Template.deserialize("be {rel} {prep}"), pattern, 1.0)
    extractor.extract(graph).map(_.toString) must contain("(Sam Gosling; be an associate professor of; psychology)")
  }

  testPostagConstraint
  testRelnounCases
  testRelRel
}
