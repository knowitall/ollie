package edu.knowitall.openparse

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.stem.MorphaStemmer

@RunWith(classOf[JUnitRunner])
object OpenParseSpecTest extends Specification {
  val openparse = OpenParse.withDefaultModel()

  "OpenParse finds an example extraction" in {
    val graph = DependencyGraph.deserialize("(._._5_37), nsubj(finds_VBZ_1_10, OpenParse_NNP_0_0); dobj(finds_VBZ_1_10, extraction_NN_4_27); det(extraction_NN_4_27, an_DT_2_16); nn(extraction_NN_4_27, example_NN_3_19)")
    val extrs = openparse.extract(graph)
    
    extrs.size must_== 1
    extrs.head._2.toString must_== "(OpenParse; finds; an example extraction)"
  }
}
