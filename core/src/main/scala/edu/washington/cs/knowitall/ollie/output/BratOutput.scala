package edu.washington.cs.knowitall.ollie.output

import edu.washington.cs.knowitall.ollie.OllieExtractionInstance
import edu.washington.cs.knowitall.openparse.extract.ExtractionPart
import edu.washington.cs.knowitall.collection.immutable.Interval

object BratOutput {
  def annotations(insts: Iterable[OllieExtractionInstance]) = {
    def partToAnnotation(inst: OllieExtractionInstance, part: ExtractionPart) = {
      val tokens = inst.sentence.nodes.toList.slice(part.interval.start, part.interval.end)
      val charInterval = Interval.open(tokens.head.offset, tokens.last.interval.end)
      "Argument " + charInterval.start + " " + charInterval.end + "\t" + tokens.mkString(" ")
    }
    
    val arguments = insts.flatMap(extr => List(extr.extr.arg1, extr.extr.arg2))
    val relations = insts.map(extr => extr.extr.rel)
  }
}