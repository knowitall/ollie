package edu.washington.cs.knowitall.ollie

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.parse.graph.Dependency
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.tool.tokenize.Tokenizer
import edu.washington.cs.knowitall.collection.immutable.graph.Graph

class DependencyGraphExtras(dgraph: DependencyGraph) {
  private def graph = dgraph.graph

  def passiveVoice: Iterable[DependencyGraph] = {
    require(dgraph.nodes forall (_.indices.length == 1))

    // look for passive constructions
    val activeVoices = this.graph.vertices.filter { v =>
      (v.postag startsWith "VB") &&
      (dgraph.dependencies exists {edge => edge.label == "nsubj" && edge.source == v}) &&
      (dgraph.dependencies exists {edge => edge.label == "dobj" && edge.source == v})
    }

    activeVoices map { v =>
      val nsubj = dgraph.dependencies.find(edge => edge.label == "nsubj" && edge.source == v).get
      val dobj = dgraph.dependencies.find(edge => edge.label == "dobj" && edge.source == v).get
      val nsubjInterval = Interval.span(dgraph.graph.inferiors(nsubj.dest).map(_.indices))
      val dobjInterval = Interval.span(dgraph.graph.inferiors(dobj.dest).map(_.indices))

      val nsubjpass = new Dependency(v, dobj.dest, "nsubjpass")

      val by = new DependencyNode("by", "IN", dobjInterval.start, -1)
      val prep = new Dependency(v, by, "prep")
      val pobj = new Dependency(by, nsubj.dest, "pobj")
      val was = new DependencyNode("was", "VBD", v.indices.start, -1)
      val auxpass = new Dependency(nsubj.source, was, "auxpass")

      // adjust the edges
      var edges: Iterable[Dependency] = dgraph.dependencies
      edges = edges.toSet - nsubj - dobj + prep + pobj + auxpass + nsubjpass
      // adjust for the "by" node
      def nodeMap = { (v: DependencyNode) =>
        var interval = v.indices
        if (v.indices.start >= by.indices.start && v != by) interval = DependencyGraphExtras.shift(interval, 1)
        if (v.indices.start >= was.indices.start && v != was) interval = DependencyGraphExtras.shift(interval, 1)
        new DependencyNode(v.text, v.postag, interval, v.offset)
      }
      edges = edges.map { e => e mapNodes nodeMap }

      edges = DependencyGraphExtras.swapOrders(edges, graph.inferiors(nsubj.dest) map nodeMap, graph.inferiors(dobj.dest) map nodeMap)

      // create the new graph
      val newGraph = new DependencyGraph(edges.flatMap(_.vertices), edges)
      val text = newGraph.nodes.iterator.map(_.text).mkString(" ")

      // compute the correct offsets
      val offsets = Tokenizer.computeOffsets(newGraph.nodes.iterator.map(_.text).toList, text)
      val nodeOffsetTransformation =
        ((newGraph.graph.vertices.iterator zip offsets.iterator) map {case (node, token) => node -> new DependencyNode(node.text, node.postag, node.indices, token.offset)}).toMap

      newGraph map nodeOffsetTransformation
    }
  }

  def activeVoice: Iterable[DependencyGraph] = {
    require(dgraph.nodes forall (_.indices.length == 1))

    // look for active constructions
    val passiveVoices = this.graph.vertices.filter { v =>
      if (!(v.postag startsWith "VB") &&
          (dgraph.dependencies exists {edge => edge.label == "nsubjpass" && edge.source == v}) &&
          (dgraph.dependencies exists (edge => edge.label == "auxpass" && edge.source == v)))
        false
      else {
        dgraph.dependencies.find(e => e.label == "prep" && e.source == v && e.dest.text == "by") match {
          case None => false
          case Some(prep) => dgraph.dependencies.exists(e => e.source == prep.dest && e.label == "pobj")
        }
      }
      (dgraph.dependencies exists {edge => edge.label == "prep" && edge.source == v})
    }

    passiveVoices map { v =>
      val nsubjpass = dgraph.dependencies.find(edge => edge.label == "nsubjpass" && edge.source == v).get
      val prep = dgraph.dependencies.find(edge => edge.label == "prep" && edge.source == v && edge.dest.text == "by" && dgraph.dependencies.exists(e => e.source == edge.dest && e.label == "pobj")).get
      val pobj = dgraph.dependencies.find(edge => edge.label == "pobj" && edge.source == prep.dest).get
      val auxpass = dgraph.dependencies.find(edge => edge.label == "auxpass" && edge.source == v).get

      val nsubj = new Dependency(v, pobj.dest, "nsubj")
      val dobj = new Dependency(v, nsubjpass.dest, "dobj")

      // adjust the edges
      var edges: Iterable[Dependency] = dgraph.dependencies
      edges = edges.toSet - nsubjpass - auxpass - prep - pobj + nsubj + dobj
      edges = DependencyGraphExtras.swapOrders(edges, graph.inferiors(nsubjpass.dest), graph.inferiors(pobj.dest))

      val nodes = scala.collection.immutable.SortedSet.empty[DependencyNode] ++ edges.flatMap(_.nodes)
      val nodeMap = nodes.iterator.zipWithIndex.map{case (node, i) => node -> new DependencyNode(node.text, node.postag, Interval.singleton(i), -1)}.toMap
      edges = edges.map(_ mapNodes nodeMap)

      // create the new graph
      val newGraph = new DependencyGraph(edges.flatMap(_.vertices), edges)
      val text = newGraph.nodes.iterator.map(_.text).mkString(" ")

      // compute the correct offsets
      val offsets = Tokenizer.computeOffsets(newGraph.nodes.iterator.map(_.text).toList, text)
      val nodeOffsetTransformation =
        ((newGraph.graph.vertices.iterator zip offsets.iterator) map {case (node, token) => node -> new DependencyNode(node.text, node.postag, node.indices, token.offset)}).toMap

      newGraph map nodeOffsetTransformation
    }
  }

  def switchVoice: Iterable[DependencyGraph] = {
    passiveVoice ++ activeVoice
  }
}

object DependencyGraphExtras {
  private def shift(interval: Interval, by: Int) = Interval.open(interval.start + by, interval.end + by)

  private def swapOrders(edges: Iterable[Dependency], left: scala.collection.Set[DependencyNode], right: scala.collection.Set[DependencyNode]) = {
    val leftInterval = Interval.span(left.map(_.indices))
    val rightInterval = Interval.span(right.map(_.indices))

    require(leftInterval.end <= rightInterval.start)

    val leftOffset = left.iterator.map(_.offset).max
    val rightOffset = right.iterator.map(_.offset).min

    val tokensBetween = rightInterval.start - leftInterval.end + 1
    val charsBetween = rightOffset - leftOffset

    edges.map(e => e.mapNodes(v =>
      if (left contains v) new DependencyNode(v.text, v.postag, DependencyGraphExtras.shift(v.indices, tokensBetween), v.offset + charsBetween)
      else if (right contains v) new DependencyNode(v.text, v.postag, DependencyGraphExtras.shift(v.indices, -tokensBetween), v.offset - charsBetween)
      else v))
  }
}