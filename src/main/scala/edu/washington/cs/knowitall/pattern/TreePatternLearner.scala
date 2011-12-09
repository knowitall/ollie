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
      val paths = graph.graph.bipaths(nodes)

      // restrict to paths that go up and then down
      paths.filter(bipath => bipath.path.length > 0 && 
          bipath.path.dropWhile(_.isInstanceOf[UpEdge[_]]).dropWhile(_.isInstanceOf[DownEdge[_]]).isEmpty)
    }
  }

  def findPattern(graph: DependencyGraph,
    lemmas: Set[String],
    replacements: Map[String, String],
    maxLength: Option[Int]) = {

    def valid(bip: Bipath[DependencyNode]) =
      // we don't have any "punct" edges
      !bip.edges.exists(_.label == "punct") &&
      // all edges are simple word characters
      bip.edges.forall(_.label.matches("\\w+")) &&
      // we have exactly one of each replacement
      replacements.keys.forall(key =>
        bip.nodes.count(node => node.text.contains(key)) == 1)

    // find paths containing lemma
    val bipaths = findBipaths(lemmas, graph, maxLength)

    // make sure each path contains exactly one of each 
    // of the replacement targets
    val filtered = bipaths.filter(valid)

    filtered.map { bip =>
      // get the indices where we need to make a replacement
      // and pair them with the replacement itself
      val rep = replacements.map {
        case (target, replacement) =>
          val index = 2 * bip.nodes.indexWhere(_.text.contains(target))
          (index, new CaptureNodeMatcher[DependencyNode](replacement))
      }

      // make the replacements
      rep.foldRight(DependencyPattern.create(bip)) {
        case (rep, pat) =>
          pat.replaceMatcherAt(rep._1, rep._2)
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
          case nm: DependencyNodeMatcher => !(rel intersect nm.label.get).isEmpty
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
          case nm: DependencyNodeMatcher => List((nm.label, index))
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

      (patternWithReplacedSlots, slotLabels)
    }
  }
}

object BuildTreePatterns {
  import TreePatternLearner._

  val logger = LoggerFactory.getLogger(this.getClass)

  val CHUNK_SIZE = 100000

  def main(args: Array[String]) {
    // file with dependencies
    val source = Source.fromFile(args(0))
    val writer = new PrintWriter(new File(args(1)))
    
    logger.info("chunk size: " + CHUNK_SIZE)

    var index = 0
    for (lines <- source.getLines.grouped(CHUNK_SIZE)) {
      @volatile var count = 0

      val ms = time(lines.par.foreach { line =>
        val Array(rel, arg1, arg2, lemmaString, text, _/*lemmas*/, _/*postags*/, _/*chunks*/, deps) = line.split("\t")
        val lemmas = lemmaString.split("\\s+").toSet

        // todo: push stemming forward in the process
        val dependencies = Dependencies.deserialize(deps).map(_.lemmatize(MorphaStemmer.instance))
        val graph = DependencyGraph(dependencies).normalize

        try {
          val patterns = findPatternsForLDA(graph, lemmas, Map(arg1 -> "arg1", arg2 -> "arg2"), rel, Some(2))
          for (pattern <- patterns) {
            val (pat, slots) = pattern
            if (slots.length == 0) {
              writer.println((List(rel, arg1, arg2, lemmas.mkString(" "), pat, text, deps) ::: slots).mkString("\t"))
              count += 1
            }
          }
        }
        catch {
          case e: NoRelationNodeException => // System.err.println(e); System.err.println(line)
        }
      })

      logger.info("chunk " + index + ": " + count + " items in " + Seconds.format(ms))
      writer.flush()

      index += 1
    }

    source.close
    writer.close
  }
}

object KeepCommonPatterns {
  def main(args: Array[String]) {
    val min = args(1).toInt

    val patterns = collection.mutable.HashMap[String, Int]().withDefaultValue(0)
    val firstSource = Source.fromFile(args(0))
    for (line <- firstSource.getLines) {
      val Array(rel, arg1, arg2, lemmas, pattern, text, deps, _*) = line.split("\t")
      patterns += pattern -> (patterns(pattern) + 1)
    }
    firstSource.close

    val secondSource = Source.fromFile(args(0))
    for (line <- secondSource.getLines) {
      val Array(rel, arg1, arg2, lemmas, pattern, text, deps, _*) = line.split("\t")
      if (patterns(pattern) >= min) {
        println(line)
      }
    }
    secondSource.close
  }
}
