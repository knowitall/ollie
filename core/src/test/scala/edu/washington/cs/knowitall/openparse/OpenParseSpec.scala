package edu.washington.cs.knowitall.openparse

import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.Specification

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer

@RunWith(classOf[JUnitSuiteRunner])
class OpenParseTest extends JUnit4(OpenParseSpec)
object OpenParseSpec extends Specification {
  val openparse = OpenParse.loadWithDefaultModel()

  "OpenParse finds an example extraction" in {
    val graph = DependencyGraph.deserialize("(._._5_37), nsubj(finds_VBZ_1_10, OpenParse_NNP_0_0); dobj(finds_VBZ_1_10, extraction_NN_4_27); det(extraction_NN_4_27, an_DT_2_16); nn(extraction_NN_4_27, example_NN_3_19)")
    val extrs = openparse.extract(graph)
    
    extrs.size must_== 1
    extrs.head._2.toString must_== "(OpenParse; finds; an example extraction)"
  }
}
