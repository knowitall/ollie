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
import tool.parse.graph.DependencyPattern

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
