package edu.washington.cs.knowitall.pattern

import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.Specification

import edu.washington.cs.knowitall.tool.parse.graph.DependencyPattern
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer.instance

@RunWith(classOf[JUnitSuiteRunner])
class ExtractorPatternTest extends JUnit4(ExtractorPatternSpec)
object ExtractorPatternSpec extends Specification {
  def testSymmetric(pattern: String, symmetric: Boolean) {
    (pattern + " is " + (if (symmetric) "symmetric" else "not symmetric")) in {
      new ExtractorPattern(DependencyPattern.deserialize(pattern)).symmetric must be_==(symmetric)
    }
  }

  testSymmetric("{arg1} <nsubj< {rel:postag=VBZ} >dobj> {arg2}", false)
  testSymmetric("{arg1} <nsubj< {rel:postag=VBD} >nsubj> {arg2}", true)
  testSymmetric("{arg1} <prep_of< {rel:postag=NN} >prep_of> {arg2}", true)
  testSymmetric("{rel:postag=NN} <nn< {arg1} >nn> {arg2}", false)
}
