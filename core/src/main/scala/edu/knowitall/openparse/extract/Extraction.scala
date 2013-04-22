package edu.knowitall.openparse.extract

import scala.collection.{SortedSet, Set}
import edu.knowitall.collection.immutable.graph.pattern.Match
import edu.knowitall.collection.immutable.graph.{Direction, Graph, DirectedEdge}
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.openparse.GraphExpansions.{expand, components, augment}
import edu.knowitall.openparse.OpenParse
import edu.knowitall.tool.parse.graph.{DependencyPattern, DependencyNode, DependencyGraph}
import edu.knowitall.ollie.Ollie.stemmer
import edu.knowitall.tool.stem.Stemmer
import Extraction.{Part, ClausalComponent, AdverbialModifier}
import edu.knowitall.tool.parse.graph.LabelEdgeMatcher
import edu.knowitall.collection.immutable.graph.pattern.DirectedEdgeMatcher

/** A representation of an OpenParse extraction.
  *
  * @author Michael Schmitz
  */
abstract class Extraction(val relLemmas: Set[String]) {
  /** the text of the first argument */
  def arg1Text: String
  /** the text of the relation */
  def relText: String
  /** the text of the second argument */
  def arg2Text: String

  def this(relText: String) = this(relText.split(" ").map(implicitly[Stemmer].lemmatize(_)).toSet -- OpenParse.LEMMA_BLACKLIST)

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

/** A simple representation of an OpenParse extraction.
  *
  * @author Michael Schmitz
  */
class SimpleExtraction(
  override val arg1Text: String,
  override val relText: String,
  relLemmas: Set[String],
  override val arg2Text: String)
  extends Extraction(relLemmas) {

  def this(arg1Text: String, relText: String, arg2Text: String) = this(arg1Text,
    relText,
    relText.split(" ").map(implicitly[Stemmer].lemmatize(_)).toSet -- OpenParse.LEMMA_BLACKLIST,
    arg2Text)

  def replaceRelation(relation: String) =
    new SimpleExtraction(this.arg1Text, this.relText, this.relLemmas, this.arg2Text)
}

/** A more informative representation of an OpenParse extraction.
  *
  * @author Michael Schmitz
  */
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

  /** all the nodes in this extraction */
  def nodes = arg1.nodes ++ rel.nodes ++ arg2.nodes

  /** all the edges in this extraction */
  def edges = `match`.bipath.path

  def replaceRelation(relation: String) =
    new DetailedExtraction(extractor, `match`, this.arg1, Part(this.rel.nodes, relation), this.arg2, this.clausal, this.modifier)
}

object DetailedExtraction {
  def nodesToString(nodes: Iterable[DependencyNode]) = nodes.iterator.map(_.text).mkString(" ")
}


/** Includes logic for expanding relations and arguments.
  *
  * @author Michael Schmitz
  */
object Extraction {
  /** Representation of a part of an extraction.
    *
    * @author Michael Schmitz
    */
  case class Part(nodes: SortedSet[DependencyNode], text: String) {
    def this(nodes: SortedSet[DependencyNode]) = {
      this(nodes, DetailedExtraction.nodesToString(nodes))
    }

    def this(nodes: Iterable[DependencyNode]) = {
      this(SortedSet[DependencyNode]() ++ nodes, DetailedExtraction.nodesToString(nodes))
    }

    def span = Interval.span(nodes.map(_.indices))
  }
  object Part {
    def connections(m: Match[DependencyNode], node: DependencyNode): Set[Graph.Edge[DependencyNode]] = {
      m.edges.filter(edge => edge.source == node || edge.dest == node).toSet
    }

    def connections(m: Match[DependencyNode], nodes: Set[DependencyNode]): Set[Graph.Edge[DependencyNode]] = {
      m.edges.filter(edge => nodes.contains(edge.source) || nodes.contains(edge.dest)).toSet
    }

    def connections(m: Match[DependencyNode], nodes: Seq[DependencyNode]): Set[Graph.Edge[DependencyNode]] = {
      m.edges.filter(edge => nodes.contains(edge.source) || nodes.contains(edge.dest)).toSet
    }
  }
  case class ClausalComponent(rel: Part, arg: Part) {
    def text = arg.text + " " + rel.text
  }
  case class AdverbialModifier(contents: Part) {
    def text = contents.text
  }

  private val attributionPattern = DependencyPattern.deserialize("{old} <ccomp< {rel} >nsubj> {arg}")
  private val conditionalPattern = DependencyPattern.deserialize("{old} <ccomp< {rel} >nsubj> {arg}")
  def fromMatch(expand: Boolean)(graph: DependencyGraph, m: Match[DependencyNode], ex: PatternExtractor): Iterable[DetailedExtraction] = {
    def clausalComponent(node: DependencyNode, until: Set[DependencyNode]) = {
      attributionPattern.apply(graph.graph, node) match {
        case List(m) =>
          assume(m.nodeGroups.get("rel").isDefined)
          assume(m.nodeGroups.get("arg").isDefined)

          val rel = m.nodeGroups("rel").node
          val arg = m.nodeGroups("arg").node

          val Part(expandedRelNodes, expandedRelText) = expandRelation(graph, rel, until + arg).head
          val expandedArg = expandArgument(graph, arg, until + rel)

          Some(ClausalComponent(Part(expandedRelNodes, expandedRelText), Part(expandedArg, DetailedExtraction.nodesToString(expandedArg))))
        case _ => None
      }
    }

    def adverbialModifier(node: DependencyNode, until: Set[DependencyNode]): Option[AdverbialModifier] = {
      val neighbors = graph.graph.neighbors(node, dedge => dedge.dir == Direction.Down && dedge.edge.label == "advcl")
      val nodes = neighbors.flatMap(graph.graph.inferiors(_))
      if (nodes.isEmpty) None
      else {
        val span = Interval.span(nodes.map(_.indices))
        val clause = graph.nodes.filter(node => span.superset(node.indices))
        Some(AdverbialModifier(Part(clause, DetailedExtraction.nodesToString(clause))))
      }
    }

    val groups = m.nodeGroups

    val rels = groups.filter(_._1 startsWith "rel").toSeq.sortBy(_._1).map(_._2.node)
    if (rels.isEmpty) (throw new IllegalArgumentException("no rel: " + m))
    val arg1 = groups.get("arg1").map(_.node) getOrElse (throw new IllegalArgumentException("no arg1: " + m))
    val arg2 = groups.get("arg2").map(_.node) getOrElse (throw new IllegalArgumentException("no arg2: " + m))

    val expandedArg1 = if (expand) expandArgument(graph, arg1, rels.toSet) else SortedSet(arg1)
    val expandedArg2 = if (expand) expandArgument(graph, arg2, rels.toSet) else SortedSet(arg2)
    val expandRels =
      // hack to exclude rel rel extractions with a second nsubj
      if (rels.size > 0 && rels.tail.exists(rel => graph.graph.dedges(rel).exists(dedge => dedge.dir == Direction.Down && dedge.edge.label == "nsubj"))) {
        Set.empty
      }
      else if (expand) {
        import scalaz._
        import Scalaz._

        val expansions = rels.map(rel => expandRelation(graph, rel, expandedArg1 ++ expandedArg2).toList).toList.sequence

        expansions.map(expansion => Part(expansion.map(_.nodes).reduce(_ ++ _), expansion.map(_.text).mkString(" ")))
      } else {
        Set(Part(SortedSet.empty[DependencyNode] ++ rels, rels.map(_.text).mkString(" ")))
      }

    for {
      Part(expandedRelNodes, expandedRelText) <- expandRels
      val nodes = expandedArg1 ++ expandedArg2 ++ expandedRelNodes
      val clausal = rels.flatMap(rel => clausalComponent(rel, nodes)).headOption
      val modifier = rels.flatMap(rel => adverbialModifier(rel, nodes)).headOption

      // arguments don't overlap
      if (!(Interval.span(expandedArg1.map(_.indices)(scala.collection.breakOut)) intersects Interval.span(expandedArg2.map(_.indices)(scala.collection.breakOut))))
    } yield (
      new DetailedExtraction(ex, m, new Part(expandedArg1), Part(expandedRelNodes, expandedRelText), new Part(expandedArg2), clausal = clausal, modifier = modifier)
    )

  }

  private val argumentExpansionLabels = Set("det", "prep_of", "amod", "num", "number", "nn", "poss", "quantmod", "neg")
  def expandArgument(graph: DependencyGraph, node: DependencyNode, until: Set[DependencyNode]): SortedSet[DependencyNode] = {
    def expandNode(node: DependencyNode) = {
      val expansion = expand(graph, node, until, argumentExpansionLabels)
      if (expansion.exists(_.isProperNoun)) expansion
      else expansion ++ components(graph, node, Set("rcmod", "infmod", "partmod", "ref", "prepc_of"), until, false).flatten
    }

    // expand over any conjunction/disjunction edges to non-verbs
    val nodes = graph.graph.connected(node, (dedge: DirectedEdge[DependencyNode]) =>
      !(dedge.end.postag startsWith "VB") && (dedge.edge.label == "conj_and" || dedge.edge.label == "conj_or"))

    if (nodes.size == 1) {
      // there are no conjunctive edges
      expandNode(node)
    }
    else {
      val flat = nodes.map(expandNode).flatten
      val span = Interval.span(flat.map(_.indices).toSeq)
      // take the nodes that cover all the nodes found
      graph.nodes.filter(node => span.superset(node.indices))
    }
  }

  /** Expand the relation nodes of a match.
    *
    * Multiple parts can be returned if there are multiple dobj or iobjs.
    *
    * @return  parts  the part (or multiple parts) that describes the relation
    */
  def expandRelation(graph: DependencyGraph, node: DependencyNode, until: Set[DependencyNode]): Set[Part] = {
    // count the adjacent dobj edges.  We will only expand across
    // dobj components if there is exactly one adjacent dobj edge.
    // This edge may already be used, but in that case we won't
    // expand over it because of the until set.
    val dobjCount = graph.graph.edges(node).count(_.label == "dobj")
    val iobjCount = graph.graph.edges(node).count(_.label == "iobj")

    var attachLabels = Set[String]()
    if (dobjCount == 1) attachLabels += "dobj"
    if (iobjCount == 1) attachLabels += "iobj"

    /*
     * acomp: "She looks beautiful on Thursday."
     */
    def pred(edge: Graph.Edge[DependencyNode]) =
      // make sure we don't re-add the relation node
      edge.dest != node && (
          // attach adverbs
          edge.label == "advmod" && edge.dest.postag == "RB" ||
          edge.label == "aux" || edge.label == "cop" || edge.label == "auxpass" || edge.label == "prt" || edge.label == "acomp")

    // expand across noun label for relational nouns
    // i.e. "He is the *best* president of the USA"
    val expandNounLabels =
      if (node.postag startsWith "NN") expand(graph, node, until, argumentExpansionLabels)
      else expand(graph, node, until, Set("det", "amod", "num", "number", "nn", "poss", "quantmod", "neg"))

    // modifiers on copulars are stored on a different node
    // i.e. in "he *will* be the president"
    val cops = graph.graph.predecessors(node, (e: Graph.Edge[DependencyNode])=>e.label == "cop").headOption
    val expandCopLabels = cops.map(cop => augment(graph, cop, until, pred)).getOrElse(List.empty)

    def f(s: Set[List[DependencyNode]]): Set[List[DependencyNode]] =
      if (s.isEmpty) Set(List())
      else s
    val dobjs = f(components(graph, node, Set("dobj"), until, true))
    val iobjs = f(components(graph, node, Set("iobj"), until, true))

    for (dobj <- dobjs; iobj <- iobjs) yield {
      val expansion = expandCopLabels ++ (expandNounLabels ::
        // make sure that we don't use a label that was
        // already captured by expandNounlabels.  This
        // can happen when a verb edges goes between two
        // noun labels.
        ((augment(graph, node, until, pred).map(_ -- expandNounLabels)) :+
          // add subcomponents
          (SortedSet[DependencyNode]() ++ dobj) :+
          (SortedSet[DependencyNode]() ++ iobj)).filterNot { c =>
            // don't add empty components
            c.isEmpty ||
              // don't add components with just "who" or "whom"
              c.size == 1 && c.headOption.map(_.postag == "WP").getOrElse(false)
          })

      val sorted = expansion.sortBy(nodes => Interval.span(nodes.map(_.indices)))

      // perform a more complicated node->text transformation
      val texts = sorted.map(DetailedExtraction.nodesToString(_))
      Part(expansion.reduce(_ ++ _), texts.mkString(" "))
    }
  }
}
