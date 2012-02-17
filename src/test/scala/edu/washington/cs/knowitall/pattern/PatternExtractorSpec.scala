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

import PatternExtractor._
import pattern.extract._

@RunWith(classOf[JUnitSuiteRunner])
class PatternExtractorSpecTest extends JUnit4(PatternExtractorSpec)
object PatternExtractorSpec extends Specification {
  def testExpandNeg {
    val sentence = "I 'm not a huge fan of John M. but these recordings do sound merakedly better than an mp3 or AAC."
    val dgraph = DependencyGraph.deserialize("nsubj(fan_NN_5, I_PRP_0); cop(fan_NN_5, 'm_VBP_1); neg(fan_NN_5, not_RB_2); det(fan_NN_5, a_DT_3); amod(fan_NN_5, huge_JJ_4); nn(M._NNP_8, John_NNP_7); prep_of(fan_NN_5, M._NNP_8); det(recordings_NNS_11, these_DT_10); nsubj(sound_VB_13, recordings_NNS_11); aux(sound_VB_13, do_VBP_12); conj_but(fan_NN_5, sound_VB_13); advmod(better_JJR_15, merakedly_RB_14); acomp(sound_VB_13, better_JJR_15); det(mp3_NN_18, an_DT_17); prep_than(better_JJR_15, mp3_NN_18); prep_than(better_JJR_15, AAC._NN_20); conj_or(mp3_NN_18, AAC._NN_20)").
        map(_.lemmatize(MorphaStemmer.instance)).normalize
    val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {arg2} >cop> {rel}")

    "not edges are moved into the relation" in {
      val extractions = new GeneralExtractor(pattern, 1, 1).extract(dgraph)
      extractions.map(_.toString) must haveTheSameElementsAs(List("(i; m; not a huge fan john m)"))
    }
  }

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
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel:VB} >prep_in> {arg2}")
      val extractions = new GeneralExtractor(pattern, 1, 1).extract(graph)
      extractions.size must_== 1
      extractions.map(_.toString) must contain("(Angels; appear; the Bible story)")
    }

    "(Angels, appear, the Bible) is not found with the wrong postag constraint" in {
      val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel:XXX} >prep_in> {arg2}")
      val extractions = new GeneralExtractor(pattern, 1, 1).extract(graph)
      extractions.size must_== 0
    }
  }

  def testRelnoun {
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

  "relation expands across 'chaotically'" in {
    val sentence = "The people fled away chaotically towards the barn."
    val pickled = "det(people_NNS_1, The_DT_0); nsubj(fled_VBD_2, people_NNS_1); advmod(fled_VBD_2, chaotically_RB_3); det(barn_NN_6, the_DT_5); prep_towards(fled_VBD_2, barn_NN_6)"
    val graph = DependencyGraph.deserialize(pickled) // don't normalize
    val pattern = DependencyPattern.deserialize("{arg1} <nsubj< {rel} >prep_towards> {arg2}")
    val extractor = new GeneralExtractor(pattern, 1, 1)
    extractor.extract(graph).map(_.toString) must haveTheSameElementsAs(List("(The people; fled chaotically; the barn)"))
  }
  
  testExpandNeg
  testPostagConstraint
  testRelnoun
}
