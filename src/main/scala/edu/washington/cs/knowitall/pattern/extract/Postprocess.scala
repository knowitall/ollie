package edu.washington.cs.knowitall.pattern.extract

import scala.collection.immutable
import edu.washington.cs.knowitall.tool.postag.PosTagger
import edu.washington.cs.knowitall.collection.immutable.Interval

object Postprocess {
  def recombine(extrs: Set[DetailedExtraction]) = {
    // keep extractions that end with a one-word preposition
    val prepositionEnding = extrs.filter { extr =>
      PosTagger.simplePrepositions(extr.rel drop (1 + extr.rel lastIndexOf ' '))
    }
      
    // break off the preposition
    val split = prepositionEnding.map { extr =>
      val preps = PosTagger.prepositions.filter(extr.rel endsWith _)
      val longest = preps.maxBy(_.length)
      (extr.rel.dropRight(longest.length + 1), longest, extr)
    }
    
    split groupBy (extr => extr._3.arg1 + ", " + extr._1) filter (_._2.size > 1) map { case (prefix, extrs) =>
      val suffixes: immutable.SortedSet[(Interval, String)] = extrs.map { case (rel, prep, extr) =>
        (Interval.span(extr.arg2Nodes.map(_.indices)), prep + " " + extr.arg2)
      }(scala.collection.breakOut)
      
      "(" + prefix + ", " + suffixes.map(_._2).mkString(", ") + ")"
    }
  }
}