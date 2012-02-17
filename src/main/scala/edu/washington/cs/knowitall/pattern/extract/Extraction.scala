package edu.washington.cs.knowitall
package pattern
package extract

import scala.Array.canBuildFrom
import scala.collection.SortedSet

import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.parse.pattern.Match
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer

import tool.parse.pattern.Match

class Extraction(
    val arg1: String, 
    val rel: String, 
    val relLemmas: Option[Set[String]], 
    val arg2: String) {

  def this(arg1: String, rel: String, arg2: String) = this(arg1, 
      rel, 
      Some(rel.split(" ").map(MorphaStemmer.instance.lemmatize(_)).toSet -- PatternExtractor.LEMMA_BLACKLIST), 
      arg2)
      
  override def equals(that: Any) = that match {
    case that: Extraction => (that canEqual this) && that.arg1 == this.arg1 && that.rel == this.rel && that.arg2 == this.arg2
    case _ => false
  }
  def canEqual(that: Any) = that.isInstanceOf[Extraction]
  override def hashCode = arg1.hashCode + 39 * (rel.hashCode + 39 * arg2.hashCode)

  override def toString() = Iterable(arg1, rel, arg2).mkString("(", "; ", ")")

  def replaceRelation(relation: String) = new Extraction(this.arg1, relation, relLemmas, this.arg2)
  def softMatch(that: Extraction) = 
    (that.arg1.contains(this.arg1) || this.arg1.contains(that.arg1)) &&
    this.relLemmas == that.relLemmas &&
    (that.arg2.contains(this.arg2) || this.arg2.contains(that.arg2))
}

class DetailedExtraction(
    val extractor: PatternExtractor,
    val `match`: Match[DependencyNode],
    val arg1Nodes: SortedSet[DependencyNode], 
    val relNodes: SortedSet[DependencyNode], 
    val relText: String,
    val arg2Nodes: SortedSet[DependencyNode])
extends Extraction(
    DetailedExtraction.nodesToString(arg1Nodes),
    relText, 
    DetailedExtraction.nodesToString(arg2Nodes)) {
  
  def this(extractor: PatternExtractor, mch: Match[DependencyNode], 
    arg1Nodes: SortedSet[DependencyNode], 
    relNodes: SortedSet[DependencyNode], 
    arg2Nodes: SortedSet[DependencyNode]) = 
    this(extractor, mch, arg1Nodes, relNodes, DetailedExtraction.nodesToString(relNodes), arg2Nodes)

  def nodes = arg1Nodes ++ relNodes ++ arg2Nodes
  def edges = `match`.bipath.path

  override def replaceRelation(relation: String) = 
    new DetailedExtraction(extractor, `match`, this.arg1Nodes, relNodes, relation, arg2Nodes)
}

object DetailedExtraction {
  def nodesToString(nodes: Iterable[DependencyNode]) = nodes.iterator.map(_.text).mkString(" ")
}
