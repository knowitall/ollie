package edu.washington.cs.knowitall.ollie.confidence

import edu.washington.cs.knowitall.ollie.OllieExtractionInstance

/** An abstract representation for a feature used by the
  * confidence function.
  *
  * @param  name  a human-readable name for this feature
  */
abstract class Feature(val name: String) {
  def apply(inst: OllieExtractionInstance): Double
}
