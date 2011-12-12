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
        val dependencies = parser.dependencies(tokens.mkString(" ")).map(dep => dep.lemmatize(MorphaStemmer.instance))
        if (lemmas forall { l => sentenceLemmas.contains(l) }) {
          val graph = DependencyGraph(dependencies).normalize
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

  def findPattern(graph: DependencyGraph,
    lemmas: Set[String],
    replacements: Map[String, String],
    maxLength: Option[Int]) = {

    def valid(bip: Bipath[DependencyNode], rep: Map[Int, ArgumentMatcher]) = {
      def params = replacements.toString+"; "+bip.toString

      // we don't have any "punct" edges
      !bip.edges.find(_.label == "punct").map { edge =>
        logger.debug("invalid: punct edge '"+edge+"': "+bip)
      }.isDefined &&
      // all edges are simple word characters
      !bip.edges.find(!_.label.matches("\\w+")).map { edge =>
        logger.debug("invalid: special character in edge '"+edge+"': "+bip)
      }.isDefined &&
      // we found replacements for everything
      !rep.find { case (index, _) => index < 0 }.map { x =>
        logger.debug("invalid: wrong number of replacements: "+params)
      }.isDefined &&
      // all replacements have a valid argument postag
      !rep.find { case (index, _) =>
        !PatternExtractor.VALID_ARG_POSTAG.contains(bip.nodes(index).postag)
      }.map { case (index, _) =>
        logger.debug("invalid: invalid arg postag '"+bip.nodes(index)+"': "+params)
      }.isDefined
    }

    // find paths containing lemma
    val bipaths = findBipaths(lemmas, graph, maxLength)

    bipaths.flatMap { bip =>
      // get the indices where we need to make a replacement
      // and pair them with the replacement itself
      val rep = replacements.map {
        case (target, replacement) =>
          // find an exact match
          val exactMatchIndex = bip.nodes.indexWhere(_.text == target)

          // otherwise, find the best match
          val index = 
            if (exactMatchIndex == -1) bip.nodes.indexWhere(_.text.contains(target))
            else exactMatchIndex

          (index, new ArgumentMatcher(replacement))
      }

      // make sure our bipath is valid
      // and we found the right replacements
      if (valid(bip, rep)) {
        // make the replacements
        val replaced = rep.foldRight(DependencyPattern.create(bip)) {
          case (rep, pat) =>
            // double the node index so we have a matcher index
            pat.replaceMatcherAt(2*rep._1, rep._2)
        }

        Some(new ExtractorPattern(replaced))
      }
      else {
        None
      }
    }
  }

  def findPatternsForLDA(graph: DependencyGraph, lemmas: Set[String], replacements: Map[String, String], rel: String, maxLength: Option[Int]) = {
    def valid(pattern: Pattern[DependencyNode]) = 
      // make sure arg1 comes first
      pattern.matchers.find(_
        .isInstanceOf[CaptureNodeMatcher[_]]).map(_
        .asInstanceOf[CaptureNodeMatcher[_]].alias == "arg1").getOrElse(false)
      
    val patterns = findPattern(graph, lemmas, replacements, maxLength)

    val filtered = patterns.filter(valid).toList

    val relStrings = rel.split(" ")

    // find the best part to replace with rel
    filtered.map { pattern =>
      val (relmatcher, relindex) = try {
        pattern.matchers.view.zipWithIndex.find(_._1 match {
          case nm: DependencyNodeMatcher => !(rel intersect nm.text.get).isEmpty
          case _ => false
        }).get
      }
      catch {
        case e: NoSuchElementException => throw new NoRelationNodeException("No relation ("+rel+") in pattern: " + pattern)
      }

      // replace rel
      val relmatcherPostag = relmatcher.asInstanceOf[DependencyNodeMatcher].postag
      val p = pattern.replaceMatcherAt(relindex, new CaptureNodeMatcher[DependencyNode]("rel:"+relmatcherPostag.get))

      // find all DependencyNodeMatchers.  These are the slots.
      val slots = p.matchers.zipWithIndex flatMap {
        case (nm, index) => nm match {
          case nm: DependencyNodeMatcher => List((nm.text, index))
          case _ => List.empty
        }
      }

      val (slotLabels, _) = slots.unzip

      val patternWithReplacedSlots = slots.zipWithIndex.map {
        case ((label, matcherIndex), slotIndex) => ("slot" + slotIndex, matcherIndex)
      }.foldRight(p) {
        case ((replacement, index), p) =>
          (p.replaceMatcherAt(index, new CaptureNodeMatcher(replacement)))
      }

      (new ExtractorPattern(patternWithReplacedSlots), slotLabels)
    }
  }
}
