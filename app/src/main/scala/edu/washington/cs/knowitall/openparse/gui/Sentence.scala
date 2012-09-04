package edu.washington.cs.knowitall.openparse.gui

import scala.util.control.Exception.catching

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph.SerializationException
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph.deserialize

/** A representation of the input sentence. */
sealed abstract class Sentence
object Sentence {
  case class Text(text: String) extends Sentence {
    override def toString = text
  }
  case class Graph(dgraph: DependencyGraph) extends Sentence {
    override def toString = dgraph.serialize
  }

  def apply(string: String): Sentence = {
    import DependencyGraph._

    catching(classOf[SerializationException]).opt {
      deserialize(string)
    } match {
      case Some(dgraph) => Graph(dgraph)
      case None => Text(string)
    }
  }
}