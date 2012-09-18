package edu.washington.cs.knowitall.ollie

import java.io.File
import java.io.PrintWriter
import scala.io.Source
import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.ollie.confidence.OllieIndependentConfFunction
import edu.washington.cs.knowitall.openparse.extract.DetailedExtraction
import edu.washington.cs.knowitall.openparse.OpenParse
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer
import scopt.OptionParser
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

/** Ollie is an Open Information Extractor that produces binary extractions
  * with context.  The constructor takes an OpenParse instance.  Ollie extends
  * OpenParse's extractions with enabling conditions and attributions.  There
  * is also a trained confidence function for [[OllieExtraction]]s.
  *
  * @author Michael Schmitz
  */
class Ollie(val openparse: OpenParse) {
  val stemmer = new MorphaStemmer

  /** Construct with the default model. */
  def this() = this(OpenParse.withDefaultModel())

  /**
    * primary method for getting extractions
    */
  def extract(dgraph: DependencyGraph): Iterable[OllieExtractionInstance] = {
    val openparseExtrs = openparse.extract(dgraph)

    for {
      (conf, extr) <- openparseExtrs
      val enabler = enablingAdverbialClauseHelper(extr)
      val attribution = attribClausalComponentHelper(extr)
    } yield new OllieExtractionInstance(
        new OllieExtraction(extr.arg1, extr.rel, extr.arg2, conf, enabler, attribution), dgraph, extr.extractor)
  }

  /** Identify enabling condition, i.e. "if it's raining..." */
  private def enablingAdverbialClauseHelper(extr: DetailedExtraction): Option[EnablingCondition] = {
    extr.modifier map { modifier =>
      val prefix = modifier.contents.nodes.head.text
      val phrase = modifier.contents.nodes.iterator.drop(1).map(_.text).mkString(" ")

      new EnablingCondition(prefix, phrase, modifier.contents.span)
    }
  }

  /** Identify attributions from clausal components, i.e. "He said..." */
  private def attribClausalComponentHelper(extr: DetailedExtraction): Option[Attribution] = {
    extr.clausal flatMap { clausal =>
      // find the first verb in the clausal rel
      clausal.rel.nodes.find(_.postag.startsWith("VB")).flatMap { node =>
        val normalized = stemmer.stem(node.text.toLowerCase())
        if (Ollie.communicationWords.contains(normalized) || Ollie.cognitiveWords.contains(normalized)) {
          val clausalArgInterval = Interval.span(clausal.arg.nodes.map(_.indices))
          val clausalRelInterval = Interval.span(clausal.rel.nodes.map(_.indices))
          Some(new Attribution(
            clausal.arg.text,
            clausal.arg.span,
            clausal.rel.text,
            clausal.rel.span))
        } else None
      }
    }
  }
}

object Ollie {
  /** A collection of verbs used for communication, i.e. "said" */
  val communicationWords = using(Source.fromInputStream(classOf[Ollie].getResource("communicationWords.txt").openStream())) { source =>
    source.getLines.toSet
  }

  /** A collection of verbs used for beliefs, i.e. "think" */
  val cognitiveWords = using(Source.fromInputStream(classOf[Ollie].getResource("cognitiveWords.txt").openStream())) { source =>
    source.getLines.toSet
  }

  /** A collection of prefixes used for enabling conditions, i.e. "if" and "when" */
  val enablerPrefixes = using(Source.fromInputStream(classOf[Ollie].getResource("prefixWords.txt").openStream())) { source =>
    source.getLines.toSet
  }
}
