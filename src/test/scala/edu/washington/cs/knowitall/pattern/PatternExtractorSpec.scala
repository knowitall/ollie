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

import OpenParse._
import pattern.extract._

@RunWith(classOf[JUnitSuiteRunner])
class PatternExtractorSpecTest extends JUnit4(PatternExtractorSpec)
object PatternExtractorSpec extends Specification {
  def testPostagConstraint {
    val sentence = "Angels appear in the Bible story from the first pages of Genesis right through to the final pages of the Book of Revelation ."
    val graph = DependencyGraph.deserialize("nsubj(appear_VB_1, Angels_NNPS_0); det(story_NN_5, the_DT_3); nn(story_NN_5, Bible_NNP_4); prep_in(appear_VB_1, story_NN_5); det(pages_NNS_9, the_DT_7); amod(pages_NNS_9, first_JJ_8); prep_from(appear_VB_1, pages_NNS_9); nn(right_NN_12, Genesis_NNP_11); prep_of(pages_NNS_9, right_NN_12); dep(appear_VB_1, through_IN_13); dep(through_IN_13, to_TO_14); det(pages_NNS_17, the_DT_15); amod(pages_NNS_17, final_JJ_16); pobj(to_TO_14, pages_NNS_17); det(Book_NNP_20, the_DT_19); prep_of(pages_NNS_17, Book_NNP_20); prep_of(Book_NNP_20, Revelation_NNP_22); punct(appear_VB_1, ._._23)").
        normalize

    "(Angels, appear, the Bible) is found without a postag constraint" in {
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel} >prep_in> {arg2}")
      val extractions = new GeneralExtractor(pattern, 1, 1).extract(graph)
      extractions.size must_== 1
      extractions.map(_.toString) must contain("(Angels; appear; the Bible story)")
    }

    "(Angels, appear, the Bible) is found with a postag constraint" in {
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel:postag=VB} >prep_in> {arg2}")
      val extractions = new GeneralExtractor(pattern, 1, 1).extract(graph)
      extractions.size must_== 1
      extractions.map(_.toString) must contain("(Angels; appear; the Bible story)")
    }

    "(Angels, appear, the Bible) is not found with the wrong postag constraint" in {
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel:postag=XXX} >prep_in> {arg2}")
      val extractions = new GeneralExtractor(pattern, 1, 1).extract(graph)
      extractions.size must_== 0
    }
  }

  def testRelnounCases {
    "adjective descriptor relnoun" in {
      val sentence = "US President Barack Obama went to the grocery store."
      val pickled = "nn(Obama_NNP_3, US_NNP_0); nn(Obama_NNP_3, President_NNP_1); nn(Obama_NNP_3, Barack_NNP_2); nsubj(went_VBD_4, Obama_NNP_3); prep(went_VBD_4, to_TO_5); det(store_NN_8, the_DT_6); nn(store_NN_8, grocery_NN_7); pobj(to_TO_5, store_NN_8)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{rel} <nn< {arg1} >nn> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; US)"))
    }

    "adjective descriptor appositive relnoun" in {
      val sentence = "Barack Obama, the US President, went to the store."
      val pickled = "nn(Obama_NNP_1, Barack_NNP_0); nsubj(went_VBD_7, Obama_NNP_1); det(President_NNP_5, the_DT_3); nn(President_NNP_5, US_NNP_4); appos(Obama_NNP_1, President_NNP_5); det(store_NN_10, the_DT_9); prep_to(went_VBD_7, store_NN_10)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel} >nn> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; US)"))
    }

    "appositive relnoun" in {
      val sentence = "Barack Obama, the president of the US, went to the grocery store."
      val pickled = "nn(Obama_NNP_1, Barack_NNP_0); nsubj(went_VBD_9, Obama_NNP_1); det(president_NN_4, the_DT_3); appos(Obama_NNP_1, president_NN_4); det(US_NNP_7, the_DT_6); prep_of(president_NN_4, US_NNP_7); det(store_NN_13, the_DT_11); nn(store_NN_13, grocery_NN_12); prep_to(went_VBD_9, store_NN_13)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel} >prep_of> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; the US)"))
    }

    "possessive relnoun" in {
      val sentence = "Barack Obama, the president of the US, went to the grocery store."
      val pickled = "nn(Obama_NNP_1, Barack_NNP_0); nsubj(went_VBD_9, Obama_NNP_1); det(president_NN_4, the_DT_3); appos(Obama_NNP_1, president_NN_4); det(US_NNP_7, the_DT_6); prep_of(president_NN_4, US_NNP_7); det(store_NN_13, the_DT_11); nn(store_NN_13, grocery_NN_12); prep_to(went_VBD_9, store_NN_13)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel} >prep_of> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; the US)"))
    }

    "possessive appositive relnoun" in {
      val sentence = "The US's president, Barack Obama, went to the store."
      val pickled = "det(US_NNP_1, The_DT_0); poss(president_NN_3, US_NNP_1); nsubj(went_VBD_8, president_NN_3); nn(Obama_NNP_6, Barack_NNP_5); appos(president_NN_3, Obama_NNP_6); det(store_NN_11, the_DT_10); prep_to(went_VBD_8, store_NN_11)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} <appos< {rel} >poss> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; The US)"))
    }

    "reverse possessive appositive relnoun" in {
      val sentence = "Barack Obama, the US's president, went to the store."
      val pickled = "nn(Obama_NNP_1, Barack_NNP_0); nsubj(went_VBD_8, Obama_NNP_1); det(US_NNP_4, the_DT_3); poss(president_NN_6, US_NNP_4); appos(Obama_NNP_1, president_NN_6); det(store_NN_11, the_DT_10); prep_to(went_VBD_8, store_NN_11)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel} >poss> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; the US)"))
    }

    "possessive is relnoun" in {
      val sentence = "The US's President is Barack Obama."
      val pickled = "det(US_NNP_1, The_DT_0); poss(President_NNP_3, US_NNP_1); nsubj(Obama_NNP_6, President_NNP_3); cop(Obama_NNP_6, is_VBZ_4); nn(Obama_NNP_6, Barack_NNP_5)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >nsubj> {rel} >poss> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; The US)"))
    }

    "is possessive relnoun" in {
      val sentence = "Barack Obama is the US's president."
      val pickled = "nn(Obama_NNP_1, Barack_NNP_0); nsubj(president_NN_6, Obama_NNP_1); cop(president_NN_6, is_VBZ_2); det(US_NNP_4, the_DT_3); poss(president_NN_6, US_NNP_4)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel} >poss> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; the US)"))
    }

    "of is relnoun" in {
      val sentence = "The president of the US is Barack Obama."
      val pickled = "det(president_NN_1, The_DT_0); nsubj(Obama_NNP_7, president_NN_1); det(US_NNP_4, the_DT_3); prep_of(president_NN_1, US_NNP_4); cop(Obama_NNP_7, is_VBZ_5); nn(Obama_NNP_7, Barack_NNP_6)"
      val graph = DependencyGraph.deserialize(pickled) // don't normalize
      val pattern = DependencyPattern.deserialize("{arg1} >nsubj> {rel} >prep_of> {arg2}")
      val extractor = new SpecificExtractor("be the president of", List("president"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; be the president of; the US)"))
    }
  }

  def testRelRel = {
    "A rel rel pattern is found" in {
      val pickled = "(of_IN_5), (._._9), nn(Obama_NNP_1, Barack_NNP_0); nsubjpass(elected_VBN_3, Obama_NNP_1); auxpass(elected_VBN_3, was_VBD_2); dobj(elected_VBN_3, president_NN_4); prep_of(president_NN_4, States_NNPS_8); det(States_NNPS_8, the_DT_6); nn(States_NNPS_8, United_NNP_7)"
      val graph = DependencyGraph.deserialize(pickled)
      val pattern = DependencyPattern.deserialize("{arg1} <nsubjpass< {rel1} >dobj> {rel2} >prep_of> {arg2}")
      val extractor = new TemplateExtractor(Template.deserialize("be {rel} of"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Barack Obama; was elected president of; the United States)"))
    }

    "A rel rel pattern is found" in {
      val pickled = "(in_IN_6), (._._8), det(Mariners_NNPS_1, The_DT_0); nsubj(team_NN_4, Mariners_NNPS_1); cop(team_NN_4, are_VBP_2); det(team_NN_4, a_DT_3); partmod(team_NN_4, located_VBN_5); prep_in(located_VBN_5, Seattle_NNP_7)"
      val graph = DependencyGraph.deserialize(pickled)
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel1} >partmod> {rel2} >prep_in> {arg2}")
      val extractor = new TemplateExtractor(Template.deserialize("be {rel} in"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(The Mariners; are a team located in; Seattle)"))
    }

    "A rel rel pattern is found" in {
      val pickled = "(._._7), nsubj(going_VBG_2, Humans_NNS_0); aux(going_VBG_2, are_VBP_1); xcomp(going_VBG_2, populate_VB_4); aux(populate_VB_4, to_TO_3); dobj(populate_VB_4, earth_NN_6); det(earth_NN_6, the_DT_5)"
      val graph = DependencyGraph.deserialize(pickled)
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel1} >xcomp> {rel2} >dobj> {arg2}")
      val extractor = new TemplateExtractor(Template.deserialize("be {rel}"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Humans; are going to populate; the earth)"))
    }

    "A rel rel pattern is found" in {
      val pickled = "(on_IN_4), (._._6), nsubj(has_VBZ_1, Juliette_NNP_0); dobj(has_VBZ_1, crush_NN_3); det(crush_NN_3, a_DT_2); prep_on(crush_NN_3, Romeo_NNP_5)"
      val graph = DependencyGraph.deserialize(pickled)
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel1} >dobj> {rel2} >prep_on> {arg2}")
      val extractor = new TemplateExtractor(Template.deserialize("{rel} on"), pattern, 1, 1)
      extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(Juliette; has a crush on; Romeo)"))
    }
  }

  "relation expands across 'chaotically'" in {
    val sentence = "The people fled away chaotically towards the barn."
    val pickled = "det(people_NNS_1, The_DT_0); nsubj(fled_VBD_2, people_NNS_1); advmod(fled_VBD_2, chaotically_RB_3); det(barn_NN_6, the_DT_5); prep_towards(fled_VBD_2, barn_NN_6)"
    val graph = DependencyGraph.deserialize(pickled) // don't normalize
    val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel} >prep_towards> {arg2}")
    val extractor = new GeneralExtractor(pattern, 1, 1)
    extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(The people; fled chaotically; the barn)"))
  }
  
  "noun relation word expands across of when it doesn't overlap with the argument" in {
    val pickled = "(of_IN_4), (by_IN_8), (of_IN_15), (at_IN_17), (of_IN_20), (at_IN_22), (and_CC_25), nsubj(types_NNS_3, These_DT_0); cop(types_NNS_3, are_VBP_1); det(types_NNS_3, the_DT_2); prep_of(types_NNS_3, clues_NNS_5); punct(types_NNS_3, ,_,_24); conj_and(types_NNS_3, team_NN_27); punct(types_NNS_3, of..._._28); partmod(clues_NNS_5, ferreted_VBN_6); prt(ferreted_VBN_6, out_RP_7); agent(ferreted_VBN_6, Gosling_NNP_10); nn(Gosling_NNP_10, Sam_NNP_9); punct(Gosling_NNP_10, ,_,_11); appos(Gosling_NNP_10, professor_NN_14); det(professor_NN_14, an_DT_12); amod(professor_NN_14, associate_JJ_13); prep_of(professor_NN_14, psychology_NN_16); prep_at(professor_NN_14, University_NNP_19); det(University_NNP_19, the_DT_18); prep_of(University_NNP_19, Texas_NNP_21); prep_at(University_NNP_19, Austin_NNP_23); poss(team_NN_27, his_PRP$_26)"
    val graph = DependencyGraph.deserialize(pickled) // don't normalize
    val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel:postag=NN} >{prep:regex=prep_(.*)}> {arg2}")
    val extractor = new TemplateExtractor(Template.deserialize("be {rel} {prep}"), pattern, 1, 1)
    extractor.extract(graph).map(_.toString) must contain("(Sam Gosling; be an associate professor of psychology at; the University of Texas)")
  }
  
  "noun relation word does NOT expands across of when it overlaps with the argument" in {
    val pickled = "(of_IN_4), (by_IN_8), (of_IN_15), (at_IN_17), (of_IN_20), (at_IN_22), (and_CC_25), nsubj(types_NNS_3, These_DT_0); cop(types_NNS_3, are_VBP_1); det(types_NNS_3, the_DT_2); prep_of(types_NNS_3, clues_NNS_5); punct(types_NNS_3, ,_,_24); conj_and(types_NNS_3, team_NN_27); punct(types_NNS_3, of..._._28); partmod(clues_NNS_5, ferreted_VBN_6); prt(ferreted_VBN_6, out_RP_7); agent(ferreted_VBN_6, Gosling_NNP_10); nn(Gosling_NNP_10, Sam_NNP_9); punct(Gosling_NNP_10, ,_,_11); appos(Gosling_NNP_10, professor_NN_14); det(professor_NN_14, an_DT_12); amod(professor_NN_14, associate_JJ_13); prep_of(professor_NN_14, psychology_NN_16); prep_at(professor_NN_14, University_NNP_19); det(University_NNP_19, the_DT_18); prep_of(University_NNP_19, Texas_NNP_21); prep_at(University_NNP_19, Austin_NNP_23); poss(team_NN_27, his_PRP$_26)"
    val graph = DependencyGraph.deserialize(pickled) // don't normalize
    val pattern = DependencyPattern.deserialize("{arg1} >appos> {rel:postag=NN} >{prep:regex=prep_(.*)}> {arg2}")
    val extractor = new TemplateExtractor(Template.deserialize("be {rel} {prep}"), pattern, 1, 1)
    extractor.extract(graph).map(_.toString) must contain("(Sam Gosling; be an associate professor of; psychology)")
  }
  
  testPostagConstraint
  testRelnounCases
  testRelRel
}