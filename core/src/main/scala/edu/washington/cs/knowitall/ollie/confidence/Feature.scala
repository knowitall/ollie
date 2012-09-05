package edu.washington.cs.knowitall.ollie.confidence

import edu.washington.cs.knowitall.ollie.OllieExtractionInstance

abstract class Feature(val name: String) {
  def apply(inst: OllieExtractionInstance): Double
}
