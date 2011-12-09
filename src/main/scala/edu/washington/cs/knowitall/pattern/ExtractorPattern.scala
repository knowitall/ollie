package edu.washington.cs.knowitall
package pattern

import tool.parse.pattern.Matcher
import tool.parse.pattern.Pattern
import tool.parse.pattern.DependencyNodeMatcher
import tool.parse.pattern.DependencyEdgeMatcher
import tool.parse.pattern.CaptureNodeMatcher
import tool.parse.graph.DependencyNode

class ExtractorPattern(matchers: List[Matcher[DependencyNode]]) extends Pattern[DependencyNode](matchers) {
  def this(pattern: Pattern[DependencyNode]) = this(pattern.matchers)
  
  def prepCount = (0 /: depEdgeMatchers) ((acc, m) => if (m.label.startsWith("prep")) 1 + acc else acc)

  def depEdgeMatchers = matchers.collect{case m: DependencyEdgeMatcher => m}
  def depNodeMatchers = matchers.collect{case m: DependencyNodeMatcher => m}
  
  def prepEdgeCount = depEdgeMatchers.count(_.label.contains("prep"))
  def symmetric = {
    def compare(m1: List[Matcher[DependencyNode]], m2: List[Matcher[DependencyNode]]) = (m1, m2) match {
      // argument matchers need not equal (in fact, they should be opposites)
      case (((c1: ArgumentMatcher) :: m1s), ((c2: ArgumentMatcher) :: m2s)) => true
      // edges and other nodes must be equal
      case (((m1: Matcher[_]) :: m1s), ((m2: Matcher[_]) :: m2s)) => m1 == m2
      case _ => false
    }
  }
}

class ArgumentMatcher(alias: String) extends CaptureNodeMatcher[DependencyNode](alias)
class RelationMatcher(alias: String) extends CaptureNodeMatcher[DependencyNode](alias)