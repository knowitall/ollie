package edu.washington.cs.knowitall
package pattern

import tool.parse.pattern.Matcher
import tool.parse.pattern.Pattern
import tool.parse.pattern.DependencyNodeMatcher
import tool.parse.pattern.DependencyEdgeMatcher
import tool.parse.pattern.LabelEdgeMatcher
import tool.parse.pattern.CaptureNodeMatcher
import tool.parse.graph.DependencyNode
import tool.parse.pattern.DependencyPattern
import tool.parse.pattern.NodeMatcher
import tool.parse.pattern.EdgeMatcher
import tool.parse.pattern.TrivialNodeMatcher

import org.slf4j.LoggerFactory

class ExtractorPattern(matchers: List[Matcher[DependencyNode]]) extends DependencyPattern(matchers) {
  val logger = LoggerFactory.getLogger(this.getClass)

  def this(pattern: Pattern[DependencyNode]) = this(pattern.matchers.map { _ match {
    case m: ExtractionPartMatcher => m
    // lift extractor matchers to a more representitive class
    case m: CaptureNodeMatcher[_] => m.alias.take(3) match {
      case "arg" => new ArgumentMatcher(m.alias)
      case "rel" => new RelationMatcher(m.alias, m.matcher)
      case "slo" => new SlotMatcher(m.alias, m.matcher)
      case _ => throw new IllegalArgumentException("Unknown capture alias: " + m.alias)
    }
    // keep everything else the same
    case m => m
  }})
  
  def valid: Boolean = {
    /* check for multiple prep edges */
    def multiplePreps = this.depEdgeMatchers.collect {
      case e: LabelEdgeMatcher => e
    }.count(_.label.contains("prep")) > 1

    /* check for a conj_and edge */
    def conjAnd = this.depEdgeMatchers.collect {
      case e: LabelEdgeMatcher => e
    }.exists(_.label == "conj_and")
    
    /* check for a conj_and edge */
    def conjOr = this.depEdgeMatchers.collect {
      case e: LabelEdgeMatcher => e
    }.exists(_.label == "conj_or")

    /* check if ends with slot */
    def slotAtEnd = {
      def isSlot(node: NodeMatcher[_]) = node match {
        case m: CaptureNodeMatcher[_] => m.alias.startsWith("slot")
        case _ => false
      }
      
      !this.nodeMatchers.isEmpty && (isSlot(this.nodeMatchers.head) || isSlot(this.nodeMatchers.last))
    }

    val length = edgeMatchers.length

    if (symmetric) {
      logger.debug("invalid: symmetric: " + this.toString)
      false
    }
    else if (length == 2 && multiplePreps) {
      logger.debug("invalid: multiple preps: " + this.toString)
      false
    }
    else if (conjAnd) {
      logger.debug("invalid: conj_and: " + this.toString)
      false
    }
    else if (conjOr) {
      logger.debug("invalid: conj_or: " + this.toString)
      false
    }
    else if (slotAtEnd) {
      logger.debug("invalid: ends with slot: " + this.toString)
      false
    }
    else {
      true
    }
  }
  
  /* determine if the pattern is symmetric, such as:
   *   {arg1} >prep> {rel} <prep< {arg2}
   */
  def symmetric = {
    def compare(m1: List[Matcher[DependencyNode]], m2: List[Matcher[DependencyNode]]): Boolean = (m1, m2) match {
      // argument matchers need not equal (in fact, they should be opposites)
      case (((c1: ArgumentMatcher) :: m1s), ((c2: ArgumentMatcher) :: m2s)) => compare(m1s, m2s)
      // edge matchers should be equals but opposite
      case (((m1: EdgeMatcher[_]) :: m1s), ((m2: EdgeMatcher[_]) :: m2s)) => m1 == m2.flip && compare(m1s, m2s)
      // edges and other nodes must be equal
      case (((m1: Matcher[_]) :: m1s), ((m2: Matcher[_]) :: m2s)) => m1 == m2 && compare(m1s, m2s)
      case (Nil, Nil) => true
      case _ => false
    }
    
    compare(matchers, matchers.reverse)
  }
}

class ExtractionPartMatcher(alias: String, matcher: NodeMatcher[DependencyNode])
extends CaptureNodeMatcher[DependencyNode](alias, matcher) {
  def this(alias: String) = this(alias, new TrivialNodeMatcher[DependencyNode])
}

class ArgumentMatcher(alias: String) extends ExtractionPartMatcher(alias)

class RelationMatcher(alias: String, matcher: NodeMatcher[DependencyNode]) 
extends ExtractionPartMatcher(alias, matcher)

class SlotMatcher(alias: String, matcher: NodeMatcher[DependencyNode]) 
extends ExtractionPartMatcher(alias, matcher)
