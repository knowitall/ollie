package edu.knowitall.openparse

import org.junit._
import org.junit.Assert._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import edu.knowitall.tool.parse.graph.DependencyPattern
import edu.knowitall.ollie.Ollie.stemmer

@RunWith(classOf[JUnitRunner])
object ExtractorPatternSpecTest extends Specification {
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
