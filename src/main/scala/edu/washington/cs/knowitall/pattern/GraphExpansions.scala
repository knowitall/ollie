package edu.washington.cs.knowitall.pattern

import scala.Option.option2Iterable
import scala.collection.Set
import scala.collection.SortedSet

import edu.washington.cs.knowitall.collection.immutable.graph.{Graph, DirectedEdge}
import edu.washington.cs.knowitall.collection.immutable.graph.Direction
import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.tool.parse.graph.{DependencyNode, DependencyGraph}

object GraphExpansions {
  def neighborsUntil(graph: DependencyGraph, node: DependencyNode, inferiors: List[DependencyNode], until: Set[DependencyNode]): SortedSet[DependencyNode] = {
    val lefts = inferiors.takeWhile(_ != node).reverse
    val rights = inferiors.dropWhile(_ != node).drop(1)

    val indices = Interval.span(node.indices :: lefts.takeWhile(!until(_)).map(_.indices) ++ rights.takeWhile(!until(_)).map(_.indices))

    // use the original dependencies nodes in case some information
    // was lost.  For example, of is collapsed into the edge prep_of
    graph.nodes.filter(node => node.indices.max >= indices.min && node.indices.max <= indices.max)
  }

  def expandAdjacent(graph: DependencyGraph, node: DependencyNode, until: Set[DependencyNode], labels: Set[String]) = {
    def takeAdjacent(interval: Interval, nodes: List[DependencyNode], pool: List[DependencyNode]): List[DependencyNode] = pool match {
      // can we add the top node?
      case head :: tail if (head.indices borders interval) && !until.contains(head) =>
        takeAdjacent(interval union head.indices, head :: nodes, tail)
      // otherwise abort
      case _ => nodes
    }

    // it might be possible to simply have an adjacency restriction
    // in this condition
    def cond(e: Graph.Edge[DependencyNode]) =
      labels.contains(e.label)
    val inferiors = graph.graph.inferiors(node, cond).toList.sortBy(_.indices)

    // split into nodes left and right of node
    val lefts = inferiors.takeWhile(_ != node).reverse
    val rights = inferiors.dropWhile(_ != node).drop(1)

    // take adjacent nodes from each list
    val withLefts = takeAdjacent(node.indices, List(node), lefts)
    val expanded = takeAdjacent(node.indices, withLefts, rights)

    SortedSet(expanded: _*)
  }

  def expand(graph: DependencyGraph, node: DependencyNode, until: Set[DependencyNode], labels: Set[String]) = {
    // don't restrict to adjacent (by interval) because prep_of, etc.
    // remove some nodes that we want to expand across.  In the end,
    // we get the span over the inferiors.  Do go beneath until
    // nodes because we need them for neighborsUntil.
    def cond(e: Graph.Edge[DependencyNode]) =
      labels.contains(e.label)
    val inferiors = graph.graph.inferiors(node, cond)
    
    // get all nodes connected by an nn edge
    val nns = graph.graph.connected(node, dedge => dedge.edge.label == "nn")
    
    // order the nodes by their indices
    val ordered = (inferiors ++ nns).toList.sortBy(_.indices)
    
    // get neighbors, moving left and right, until a bad node is it
    neighborsUntil(graph, node, ordered, until)
  }

  def augment(graph: DependencyGraph, node: DependencyNode, without: Set[DependencyNode], pred: Graph.Edge[DependencyNode] => Boolean): List[SortedSet[DependencyNode]] = {
    // don't restrict to adjacent (by interval) because prep_of, etc.
    // remove some nodes that we want to expand across.  In the end,
    // we get the span over the inferiors.
    graph.graph.successors(node, pred).map { successor =>
      SortedSet[DependencyNode]() ++ graph.graph.inferiors(successor)
    }.toList
  }

  /**
    *  Find all nodes in a components next to the node.
    *  @param  node  components will be found adjacent to this node
    *  @param  labels  components may be connected by edges with any of these labels
    *  @param  without  components may not include any of these nodes
    */
  def components(graph: DependencyGraph, node: DependencyNode, labels: Set[String], without: Set[DependencyNode], nested: Boolean) = {
    // nodes across an allowed label to a subcomponent
    val across = graph.graph.neighbors(node, (dedge: DirectedEdge[_]) => dedge.dir match {
      case Direction.Down if labels.contains(dedge.edge.label) => true
      case _ => false
    })

    val components = across.flatMap { start =>
      // get inferiors without passing back to node
      val inferiors = graph.graph.inferiors(start,
        (e: Graph.Edge[DependencyNode]) =>
          // make sure we don't cycle out of the component
          e.dest != node &&
            // make sure we don't descend into another component
            // i.e. "John M. Synge who came to us with his play direct
            // from the Aran Islands , where the material for most of
            // his later works was gathered" if nested is false
            (nested || !labels.contains(e.label)))

      // make sure none of the without nodes are in the component
      if (without.forall(!inferiors.contains(_))) {
        val span = Interval.span(inferiors.map(_.indices).toSeq)
        Some(graph.nodes.filter(node => span.superset(node.indices)))
      }
      else None
    }

    components.flatten.toList
  }
}