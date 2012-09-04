package edu.washington.cs.knowitall.openparse.template

import java.io.{PrintWriter, File}
import scala.Option.option2Iterable
import scala.annotation.elidable
import scala.collection.immutable
import scala.io.Source
import scala.util.matching.Regex
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.collection.immutable.graph.pattern.{Matcher, ConjunctiveNodeMatcher, DirectedEdgeMatcher, CaptureEdgeMatcher}
import edu.washington.cs.knowitall.collection.immutable.Bag
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.common.enrich.Traversables.traversableOncePairTo
import edu.washington.cs.knowitall.openparse.{SlotMatcher, RelationMatcher, ExtractorPattern, ArgumentMatcher}
import edu.washington.cs.knowitall.tool.parse.graph.{RegexNodeMatcher, RegexEdgeMatcher, PostagNodeMatcher, LabelEdgeMatcher, DependencyPattern, DependencyNode}
import edu.washington.cs.knowitall.tool.postag.Postagger
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer.instance
import scalaz.Scalaz._
import scalaz._
import scopt.OptionParser
import edu.washington.cs.knowitall.collection.immutable.graph.pattern.CaptureNodeMatcher

/** A main method for adding active and passive conversions
  * of patterns to a model file.  BuiltTemplates removes
  * duplicate patterns, removing a lot of garbage but also
  * the active/passive conversions.
  *
  * @author Michael Schmitz
  */
object PassiveReflections {
  val logger = LoggerFactory.getLogger(this.getClass)

  abstract class Settings {
    def sourceFile: File
    def destFile: Option[File]
  }

  def main(args: Array[String]) {
    val settings = new Settings {
      var sourceFile: File = null
      var destFile: Option[File] = None
    }

    val parser = new OptionParser("passivemodel") {
      arg("source", "input model file", { path: String => settings.sourceFile = new File(path) })
      argOpt("dest", "output model file", { path: String => settings.destFile = Some(new File(path)) })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }

  def run(settings: Settings) {
    def switchArgs(pattern: ExtractorPattern) = {
      val arg1 = pattern.matchers.find { case m: CaptureNodeMatcher[_] => m.alias == "arg1" case _ => false } get
      val arg2 = pattern.matchers.find { case m: CaptureNodeMatcher[_] => m.alias == "arg2" case _ => false } get

      new ExtractorPattern(pattern.matchers.map {
        case m: CaptureNodeMatcher[_] if m.alias == "arg1" => arg2
        case m: CaptureNodeMatcher[_] if m.alias == "arg2" => arg1
        case m => m
      })
    }

    val patterns = using {
      Source.fromFile(settings.sourceFile)
    } { source =>
      source.getLines.drop(1).map { line =>
        val Array(template, pattern, count) = line.split("\t")
        (template, new ExtractorPattern(DependencyPattern.deserialize(pattern)), count)
      }.toList
    }

    using(
      settings.destFile match {
        case Some(file) => new PrintWriter(file, "UTF8")
        case None => new PrintWriter(System.out)
      }) { output =>
        patterns.foreach {
          case (template, pattern, count) =>
            output.println(Iterable(template, pattern, count).mkString("\t"))

            if (pattern.baseEdgeMatchers.exists { case m: LabelEdgeMatcher => m.label == "nsubj" case _ => false }) {
              // print the passive conversion

              if (!(template startsWith "be ")) {
                output.println(Iterable("be " + template, switchArgs(pattern), count).mkString("\t"))
              }
            } else if (pattern.baseEdgeMatchers.exists { case m: LabelEdgeMatcher => m.label == "nsubjpass" case _ => false }) {
              if (template startsWith "be ") {
                output.println(Iterable(template.drop(3), switchArgs(pattern), count).mkString("\t"))
              }
            }
        }
      }
  }
}