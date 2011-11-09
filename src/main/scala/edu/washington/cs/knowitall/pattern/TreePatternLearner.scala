package edu.washington.cs.knowitall
package parse
package pattern

import java.io.File
import java.io.PrintWriter

import scala.io.Source

import TreePatternLearner.findPattern
import edu.washington.cs.knowitall.util.DefaultObjects
import tool.parse.graph._
import tool.parse.pattern._
import tool.parse.StanfordParser
import tool.stem.MorphaStemmer

object TreePatternLearner {
  def main(args: Array[String]) {
    def toString(pattern: Pattern, arg1: String, arg2: String) = {
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
        .map(w => MorphaStemmer.instance.stem(w)))
    val lemmas = Set(arg1 ++ rel ++ arg2 :_*);

    val tokenizer = DefaultObjects.getDefaultTokenizer

    dest.println(lemmas.mkString(" "))
    println(relation.mkString(", "))

    val map = new collection.mutable.HashMap[String, collection.mutable.Set[String]]() with collection.mutable.MultiMap[String, String]
    source.foreach { line =>
      if (line.length < 500) {
        val tokens = tokenizer.tokenize(line)
        val sentenceLemmas = tokens.map(tok => MorphaStemmer.instance.stem(tok))
        val nodes = sentenceLemmas.zipWithIndex.map { case (lemma, index) => new DependencyNode(lemma, lemma, index) }
        val dependencies = parser.dependencies(tokens.mkString(" ")).map(dep => dep.normalize(MorphaStemmer.instance))
        if (lemmas forall { l => sentenceLemmas.contains(l) }) {
          val graph = new DependencyGraph(line, dependencies).collapseNounGroups.collapseNNPOf
          val patterns = findPattern(lemmas, Map(arg1.mkString(" ") -> "arg1", arg2.mkString(" ") -> "arg2"), graph)
            .filter(_.matchers.find(_
                    .isInstanceOf[CaptureNodeMatcher]).map(_
                        .asInstanceOf[CaptureNodeMatcher].alias == "arg1").getOrElse(false))
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
  
  def findBipath(lemmas: Set[String], graph: DependencyGraph) = {
    // build a set of all the possible combinations that don't use a
    // node with the same text twice
    def combinations(nodes: Set[DependencyNode]) = {
      def rec(nodes: Seq[(String,Set[DependencyNode])], combs: Set[DependencyNode] ): Set[Set[DependencyNode]] = nodes match {
        case Seq((text, set), rest @ _*) => set.flatMap(item => rec(rest, combs + item))
        case Seq() => Set(combs)
      }
      
      if (nodes.map(_.text).size == nodes.size) {
        // there are no duplicate nodes
        Set(nodes)
      }
      else {
        // build combinations
        rec(nodes.groupBy(_.text).toSeq, Set())
      }
    }
    
    val allNodes = lemmas.flatMap(
        lemma => {
          // find all exact matches
          val exacts = graph.vertices.filter(node => node.text == lemma)
          
          // or one partial match
          if (exacts.isEmpty) graph.vertices.find(node => node.text.contains(lemma)) map { List(_) } getOrElse List.empty
          else exacts
        })
    combinations(allNodes).flatMap{ nodes =>
      val paths = graph.edgeBipaths(nodes)
    
      // restrict to paths that go up and then down
      paths.filter(bipath => bipath.path.length > 0 && bipath.path.dropWhile(_.isInstanceOf[UpEdge]).dropWhile(_.isInstanceOf[DownEdge]).isEmpty)
    }
  }

  def findPattern(lemmas: Set[String], replacements: Map[String, String], graph: DependencyGraph) = {
    // find paths containing lemma
    val bipaths = findBipath(lemmas, graph)
      // make sure each path contains exactly one of each of the replacement targets
      .filter(bip => replacements.keys.forall(arg => bip.nodes.count(node => node.text.contains(arg)) == 1))
      
    bipaths.map { bip =>
        val rep = replacements.map { case (target, replacement) => 
          (bip.nodes.indexWhere( _.text.contains(target)), new CaptureNodeMatcher(replacement)) }
        rep.foldRight(new Pattern(bip)) { case(rep, pat) =>
          pat.replaceNodeMatcherAtIndex(rep._1, rep._2)
        }
    }
  }

  def findPatternsForLDA(lemmas: Set[String], replacements: Map[String, String], rel: String, graph: DependencyGraph) = {
    // arg1 comes first
    val patterns = findPattern(lemmas, replacements, graph)
      .filter(_
        .matchers.find(_
          .isInstanceOf[CaptureNodeMatcher]).map(_
          .asInstanceOf[CaptureNodeMatcher].alias == "arg1").getOrElse(false)).toList
          
    // find the best part to replace with rel
    patterns.map { pattern => 
      val relindex = pattern.matchers.indexWhere (_ match {
        case nm: DefaultNodeMatcher => rel.contains(nm.label)
        case _ => false
      })
      
      val p = pattern.replaceNodeMatcherAtIndex(relindex, new CaptureNodeMatcher("rel"))
      val slots = p.matchers.zipWithIndex flatMap {
        case (nm, index) => nm match {
          case nm: DefaultNodeMatcher => List((nm.label, index))
          case _ => List.empty
        }
      }
      
      val (slotLabels, _) = slots.unzip
      (slots.zipWithIndex.map { 
        case ((label, index), i) => ("slot" + i, index)
      }.foldRight(p) { case ((replacement, index), p) =>
        (p.replaceNodeMatcherAtIndex(index, new CaptureNodeMatcher(replacement)))
      }, slotLabels)
    }
  }
}

object TreePatternConsole {
  import TreePatternLearner._
  
  def main(args: Array[String]) {
    val source =
      if (args.length > 0) Source.fromFile(args(0)).getLines
      else Source.stdin.getLines

    for (line <- source) {
      val Array(arg1, rel, arg2, lemmaString, deps) = line.split("\t")
      val lemmas = lemmaString.split("\\s+").toSet
      
      val dependencies = Dependencies.deserialize(deps).map(_.normalize(MorphaStemmer.instance))
      val graph = new DependencyGraph(line, dependencies).collapseNounGroups.collapseNNPOf
      
      val patterns = findPatternsForLDA(lemmas, Map(arg1 -> "arg1", arg2 -> "arg2"), rel, graph)
      if (patterns.size > 0) {
        println((rel :: arg1 :: arg2 :: lemmas :: patterns.toList).mkString("\t"))
      }
    }
  }
}
