package edu.washington.cs.knowitall
package pattern

import java.io.File
import java.io.PrintWriter

import scala.io.Source
import scala.collection

import common.Timing._

import TreePatternLearner.findPattern

import tool.parse._
import tool.stem._
import tool.parse.pattern._
import tool.parse.graph._
import util.DefaultObjects

import org.slf4j.LoggerFactory

object TreePatternLearner {
  val logger = LoggerFactory.getLogger(this.getClass)
  
  class NoRelationNodeException(message: String) extends NoSuchElementException(message)
  
  def main(args: Array[String]) {
    def toString(pattern: Pattern[DependencyNode], arg1: String, arg2: String) = {
      pattern.toString
    }

    val source =
      if (args.length > 0) Source.fromFile(args(0)).getLines
      else Source.stdin.getLines

    val dest =
      if (args.length > 1) new PrintWriter(new File(args(1)))
      else new PrintWriter(System.out)

    val parser = new StanfordParser()

    val relation = source.next().split(",") //.map(w => MorphaStemmer.instance.normalize(w))
    val Array(arg1, rel, arg2) = relation
      .map(part => part.split("""\s+""")
        .map(w => MorphaStemmer.instance.lemmatize(w)))
    val lemmas = Set(arg1 ++ rel ++ arg2: _*);

    val tokenizer = DefaultObjects.getDefaultTokenizer

    val map = new collection.mutable.HashMap[String, collection.mutable.Set[String]]() with collection.mutable.MultiMap[String, String]
    source.foreach { line =>
      if (line.length < 500) {
        val tokens = tokenizer.tokenize(line)
        val sentenceLemmas = tokens.map(tok => MorphaStemmer.instance.lemmatize(tok))
        val nodes = sentenceLemmas.zipWithIndex.map { case (lemma, index) => new DependencyNode(lemma, lemma, index) }
        val graph = parser.dependencyGraph(tokens.mkString(" ")).map(dep => dep.lemmatize(MorphaStemmer.instance))
        if (lemmas forall { l => sentenceLemmas.contains(l) }) {
          val patterns = findPattern(graph, lemmas, Map(arg1.mkString(" ") -> "arg1", arg2.mkString(" ") -> "arg2"), None)
            .filter(_.matchers.find(_
              .isInstanceOf[CaptureNodeMatcher[_]]).map(_
              .asInstanceOf[CaptureNodeMatcher[_]].alias == "arg1").getOrElse(false))
          for (pattern <- patterns) {
            map.addBinding(toString(pattern, arg1.mkString(" "), arg2.mkString(" ")), line)
          }
        }
      }
    }

    (map.toList sortBy { case (key, values) => values.size }).reverse.take(20).
      foreach { case (key, values) => dest.println(values.size + "\t" + key + "\t" + values.mkString("\t")) }

    dest.close
  }

  def findBipaths(lemmas: Set[String], graph: DependencyGraph, maxLength: Option[Int]) = {
    // build a set of all the possible combinations that don't use a
    // node with the same text twice
    def combinations(nodes: Set[DependencyNode]) = {
      def rec(nodes: Seq[(String, Set[DependencyNode])], combs: Set[DependencyNode]): Set[Set[DependencyNode]] = nodes match {
        case Seq((text, set), rest @ _*) => set.flatMap(item => rec(rest, combs + item))
        case Seq() => Set(combs)
      }

      if (nodes.map(_.text).size == nodes.size) {
        // there are no duplicate nodes
        Set(nodes)
      } else {
        // build combinations
        rec(nodes.groupBy(_.text).toSeq, Set())
      }
    }

    val allNodes = lemmas.flatMap { lemma =>
      // find all exact matches
      val exacts = graph.graph.vertices.filter(_.text == lemma)

      // or one partial match
      if (exacts.isEmpty) graph.graph.vertices.find(_.text.contains(lemma)) map { List(_) } getOrElse List.empty
      else exacts
    }

    combinations(allNodes).flatMap { nodes =>
      val paths = graph.graph.bipaths(nodes, maxLength)

      // restrict to paths that go up and then down
      paths.filter(bipath => bipath.path.length > 0 && 
          bipath.path.dropWhile(_.isInstanceOf[UpEdge[_]]).dropWhile(_.isInstanceOf[DownEdge[_]]).isEmpty)
    }
  }

  class InvalidBipathException(message: String) extends RuntimeException(message)
  def findPattern(graph: DependencyGraph,
    lemmas: Set[String],
    replacements: Map[String, String],
    maxLength: Option[Int]) = {

    def valid(bip: Bipath[DependencyNode]) = {
      def params = replacements.toString+"; "+bip.toString

      // we don't have any "punct" edges
      !bip.edges.find(_.label == "punct").map { edge =>
        logger.debug("invalid: punct edge '"+edge+"': "+bip)
      }.isDefined &&
      // we don't have any "dep" edges
      !bip.edges.find(_.label == "dep").map { edge =>
        logger.debug("invalid: dep edge '"+edge+"': "+bip)
      }.isDefined &&
      // all edges are simple word characters
      !bip.edges.find(!_.label.matches("\\w+")).map { edge =>
        logger.debug("invalid: special character in edge '"+edge+"': "+bip)
      }.isDefined
    }

    // find paths containing lemma
    val bipaths = findBipaths(lemmas, graph, maxLength) filter valid

    bipaths.flatMap { bip =>
      import scalaz._
      import Scalaz._

      val zipper = DependencyPattern.create(bip).matchers.toZipper.get

      val zipperReplaced = try { Some((zipper /: replacements){ 
        case (zipper, (target, rep)) =>
          val zipperMatch = 
            // find an exact match
            zipper.findZ {
              case m: DependencyNodeMatcher => m.text.get == target
              case _ => false
           } orElse
            // find a partial match
            zipper.findZ {
              case m: DependencyNodeMatcher => m.text.get contains target
              case _ => false
            } getOrElse {
              throw new InvalidBipathException("invalid: couldn't find replacement '"+rep+"': "+bip)
            }

          // ensure valid postags
          if (!PatternExtractor.VALID_ARG_POSTAG.contains(zipperMatch.focus.asInstanceOf[DependencyNodeMatcher].postag.get))
            throw new InvalidBipathException("invalid: invalid arg postag '"+zipper.focus+"': "+bip)

          // make replacements
          Scalaz.zipper(zipperMatch.lefts, new ArgumentMatcher(rep), zipperMatch.rights)
      })} catch {
        case e: InvalidBipathException => logger.debug(e.getMessage); None
      }

      zipperReplaced.map(zipper => new ExtractorPattern(zipper.toStream.toList))
    }
  }

  def findPatternsForLDA(graph: DependencyGraph, lemmas: Set[String], replacements: Map[String, String], rel: String, maxLength: Option[Int]): List[(ExtractorPattern, List[String])] = {
    def valid(pattern: Pattern[DependencyNode]) = 
      // make sure arg1 comes first
      pattern.matchers.find(_
        .isInstanceOf[CaptureNodeMatcher[_]]).map(_
        .asInstanceOf[CaptureNodeMatcher[_]].alias == "arg1").getOrElse(false)
      
    val patterns = findPattern(graph, lemmas, replacements, maxLength)
    
    val filtered = patterns.filter(valid).toList

    val relLemmas = rel.split(" ").toSet -- PatternExtractor.LEMMA_BLACKLIST

    // find the best part to replace with rel
    filtered.map { pattern =>
      import scalaz._
      import Scalaz._

      def replaceRel(zipper: Zipper[Matcher[DependencyNode]]) = {
        // find the rel node
        val relZipper = zipper.findZ(_ match {
          case nm: DependencyNodeMatcher => !(relLemmas intersect nm.text.get.split(" ").toSet).isEmpty
          case _ => false
        }) getOrElse {
          throw new NoRelationNodeException("No relation ("+rel+") in pattern: " + pattern)
        }

        // replace rel
        val postag = relZipper.focus.asInstanceOf[DependencyNodeMatcher].postag.get
        Scalaz.zipper[Matcher[DependencyNode]](relZipper.lefts, new CaptureNodeMatcher("rel:"+postag), relZipper.rights)
      }

      def replaceSlots(zipper: Zipper[Matcher[DependencyNode]]) = {
        def replaceSlots(zipper: Zipper[Matcher[DependencyNode]], labels: List[String], index: Int): (Zipper[Matcher[DependencyNode]], List[String]) = {
          def replaceSlot(zipper: Zipper[Matcher[DependencyNode]]) = {
            val node = zipper.focus.asInstanceOf[DependencyNodeMatcher]
            val postag = node.postag.get
            (Scalaz.zipper[Matcher[DependencyNode]](zipper.lefts, new CaptureNodeMatcher("slot"+index), zipper.rights),
                node.text.get)
          }

          zipper.findZ(_.isInstanceOf[DependencyNodeMatcher]) match {
            case Some(z) => val (zipper, label) = replaceSlot(z)
                    replaceSlots(zipper, label :: labels, index + 1)
            case None => (zipper, labels)
          }
        }

        replaceSlots(zipper, List(), 0)
      }

      val zipper = pattern.matchers.toZipper.get
      val relZipper = replaceRel(zipper)
      val (slotZipper, slotLabels) = replaceSlots(relZipper)

      (new ExtractorPattern(slotZipper.toStream.toList), slotLabels)
    }
  }
}
