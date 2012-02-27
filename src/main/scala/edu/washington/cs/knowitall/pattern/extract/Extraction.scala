package edu.washington.cs.knowitall
package pattern
package extract

import scala.Array.canBuildFrom
import scala.collection.SortedSet

import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.parse.pattern.Match
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer

import tool.parse.pattern.Match

import Extraction._

abstract class Extraction(val relLemmas: Set[String]) {
  def arg1Text: String
  def relText: String
  def arg2Text: String
  
  def this(relText: String) = this(relText.split(" ").map(MorphaStemmer.instance.lemmatize(_)).toSet -- OpenParse.LEMMA_BLACKLIST)

  override def equals(that: Any) = that match {
    case that: Extraction => (that canEqual this) && that.arg1Text == this.arg1Text && that.relText == this.relText && that.arg2Text == this.arg2Text
    case _ => false
  }
  def canEqual(that: Any) = that.isInstanceOf[Extraction]
  override def hashCode = arg1Text.hashCode + 39 * (relText.hashCode + 39 * arg2Text.hashCode)

  override def toString() = Iterable(arg1Text, relText, arg2Text).mkString("(", "; ", ")")

  def softMatch(that: Extraction) = 
    (that.arg1Text.contains(this.arg1Text) || this.arg1Text.contains(that.arg1Text)) &&
    this.relLemmas == that.relLemmas &&
    (that.arg2Text.contains(this.arg2Text) || this.arg2Text.contains(that.arg2Text))
}

class SimpleExtraction (
    override val arg1Text: String, 
    override val relText: String, 
    relLemmas: Set[String], 
    override val arg2Text: String) 
extends Extraction(relLemmas) {
  
  def this(arg1Text: String, relText: String, arg2Text: String) = this(arg1Text, 
      relText, 
      relText.split(" ").map(MorphaStemmer.instance.lemmatize(_)).toSet -- OpenParse.LEMMA_BLACKLIST, 
      arg2Text)
      
  def replaceRelation(relation: String) = 
    new SimpleExtraction(this.arg1Text, this.relText, this.relLemmas, this.arg2Text)
}

class DetailedExtraction(
    val extractor: PatternExtractor,
    val `match`: Match[DependencyNode],
    val arg1: Part, 
    val rel: Part, 
    val arg2: Part,
    val clausal: Option[ClausalComponent] = None,
    val modifier: Option[AdverbialModifier] = None)
extends Extraction(rel.text) {
  
  override def arg1Text = arg1.text
  override def relText = rel.text
  override def arg2Text = arg2.text
  
  def this(extractor: PatternExtractor, mch: Match[DependencyNode], 
    arg1Nodes: SortedSet[DependencyNode], 
    relNodes: SortedSet[DependencyNode], 
    arg2Nodes: SortedSet[DependencyNode]) = 
    this(extractor, mch, new Part(arg1Nodes), new Part(relNodes), new Part(arg2Nodes))

  def nodes = arg1.nodes ++ rel.nodes ++ arg2.nodes
  def edges = `match`.bipath.path

  def replaceRelation(relation: String) = 
    new DetailedExtraction(extractor, `match`, this.arg1, Part(this.rel.nodes, relation), this.arg2, this.clausal, this.modifier)
}

object DetailedExtraction {
  def nodesToString(nodes: Iterable[DependencyNode]) = nodes.iterator.map(_.text).mkString(" ")
}

object Extraction {
  case class Part(nodes: SortedSet[DependencyNode], text: String) {
    def this(nodes: SortedSet[DependencyNode]) = {
      this(nodes, DetailedExtraction.nodesToString(nodes))
    }
    
    def this(nodes: Iterable[DependencyNode]) = {
      this(SortedSet[DependencyNode]() ++ nodes, DetailedExtraction.nodesToString(nodes))
    }
  }
  case class ClausalComponent(rel: Part, arg: Part) {
    def text = arg.text + " " + rel.text
  }
  case class AdverbialModifier(contents: Part) {
    def text = contents.text
  }
}
