package edu.washington.cs.knowitall.openparse

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer
import edu.washington.cs.knowitall.ollie.Ollie
import edu.washington.cs.knowitall.ollie.OllieExtractionInstance
import edu.washington.cs.knowitall.ollie.ScoredOllieExtractionInstance

@RunWith(classOf[JUnitRunner])
object OllieSpecTest extends Specification {
  val ollie = new Ollie(OpenParse.withDefaultModel())

  "OpenParse finds an example extraction" in {
    val graph = DependencyGraph.deserialize("(._._5_37), nsubj(finds_VBZ_1_10, OpenParse_NNP_0_0); dobj(finds_VBZ_1_10, extraction_NN_4_27); det(extraction_NN_4_27, an_DT_2_16); nn(extraction_NN_4_27, example_NN_3_19)")
    val extrs = ollie.extract(graph)

    val extr = extrs.head
    extr must_== OllieExtractionInstance.tabDeserialize(extr.tabSerialize)

    val scored = new ScoredOllieExtractionInstance(true, extr)
    scored must_== ScoredOllieExtractionInstance.tabDeserialize(scored.tabSerialize)
  }
}
