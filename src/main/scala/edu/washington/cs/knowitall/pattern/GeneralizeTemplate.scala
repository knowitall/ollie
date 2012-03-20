package edu.washington.cs.knowitall.pattern

import java.io.File
import java.io.PrintWriter
import scala.collection.immutable
import scala.collection.immutable
import scala.io.Source
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.common.enrich.Traversables._
import edu.washington.cs.knowitall.common.enrich.Traversables.traversableOncePairIntTo
import edu.washington.cs.knowitall.common.enrich.Traversables.traversableOnceTo
import edu.washington.cs.knowitall.tool.parse.pattern.CaptureEdgeMatcher
import edu.washington.cs.knowitall.tool.parse.pattern.ConjunctiveNodeMatcher
import edu.washington.cs.knowitall.tool.parse.pattern.DependencyPattern
import edu.washington.cs.knowitall.tool.parse.pattern.DirectedEdgeMatcher
import edu.washington.cs.knowitall.tool.parse.pattern.EdgeMatcher
import edu.washington.cs.knowitall.tool.parse.pattern.LabelEdgeMatcher
import edu.washington.cs.knowitall.tool.parse.pattern.Matcher
import edu.washington.cs.knowitall.tool.parse.pattern.RegexEdgeMatcher
import edu.washington.cs.knowitall.tool.parse.pattern.RegexNodeMatcher
import scalaz.Scalaz._
import scopt.OptionParser
import edu.washington.cs.knowitall.tool.parse.pattern.PostagNodeMatcher
import edu.washington.cs.knowitall.tool.parse.pattern.TrivialNodeMatcher
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.parse.pattern.NodeMatcher
import scala.collection.immutable

object GeneralizeTemplates {
  abstract class Settings {
    def sourceFile: File
    def destFile: Option[File]

    val categories = List("person", "location")
  }

  def main(args: Array[String]) = {
    object settings extends Settings {
      var sourceFile: File = null
      var destFile: Option[File] = None
    }

    val parser = new OptionParser("buildtemp") {
      arg("source", "file with source relation, pattern pairs", { path: String => settings.sourceFile = new File(path) })
      argOpt("dest", "optional parameter to specify output to a file", { path: String => settings.destFile = Some(new File(path)) })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }

  def lexicalRestrictions(extractionPartMatcher: ExtractionPartMatcher) = {
    extractionPartMatcher.matcher match {
      case m: ConjunctiveNodeMatcher[_] =>
        val postag = (m.matchers.collect { case m: PostagNodeMatcher => m } head).postag
        val lemmas = (m.matchers.collect { case m: RegexNodeMatcher => m } head).regex.toString.split("\\|").toSeq
        Some(postag, lemmas)
      case _ => None
    }
  }

  case class Category(name: String, elements: Set[String]) {
    override def toString = "Category(" + name + ")"
  }

  def loadCategories(categories: Seq[String]) = {
    def loadCategory(name: String) = {
      val elements =
        using(this.getClass.getClassLoader.getResourceAsStream("categories/" + name + ".txt")) { stream =>
          using(Source.fromInputStream(stream)) { source =>
            source.getLines().toSet
          }
        }

      Category(name, elements)
    }

    (for (cat <- categories) yield (loadCategory(cat))).toList
  }

  def run(settings: Settings) {
    val categories = loadCategories(settings.categories)

    def generalize(matcher: NodeMatcher[DependencyNode], postag: String, lemmas: Set[String]) = {
      def distance(cat: Category) = (cat.elements intersect lemmas).size.toDouble / lemmas.size.toDouble
      if (lemmas.size < 10) matcher
      else {
        postag match {
          case "NN" | "NNS" =>
            val overlaps = categories map (cat => (cat, distance(cat))) sortBy (-_._2)
            if (overlaps.iterator.map(_._2).sum > 0.50) {
              val categories = overlaps.filter(_._2 > 0.10).map(_._1)
              val uncategorized = lemmas -- categories.flatMap(_.elements)
              val elements = immutable.SortedSet[String]() ++ categories.flatMap(_.elements) ++ uncategorized
              new ConjunctiveNodeMatcher(new PostagNodeMatcher(postag), new RegexNodeMatcher(elements.mkString("|").r))
            } else matcher
          case m => matcher
        }
      }
    }

    var templates =
      using(Source.fromFile(settings.sourceFile)) { source =>
        source.getLines().map { line =>
          val Array(template, pattern, count) = line.split("\t")
          ((template, new ExtractorPattern(DependencyPattern.deserialize(pattern))), count.toInt)
        }.toList
      }

    templates = templates.map {
      case ((template, pattern), count) =>
        val matchers = pattern.matchers.map { matcher =>
          matcher match {
            case m: ExtractionPartMatcher =>
              lexicalRestrictions(m) match {
                case Some((postag, lemmas)) => m.withMatcher(generalize(m.matcher, postag, lemmas.toSet))
                case None => m
              }
            case m => m
          }
        }

        ((template, new ExtractorPattern(matchers)), count)
    }
    
    using (
      settings.destFile match {
        case Some(file) => new PrintWriter(file)
        case None => new PrintWriter(System.out)
      }) 
    { writer =>
      templates map { case ((template, pattern), count) => Iterable(template, pattern, count).mkString("\t") } foreach writer.println
    }
  }
}
