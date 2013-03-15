package edu.knowitall.openparse.gui

import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.parse.MaltParser
import edu.knowitall.tool.parse.graph.Dependencies
import edu.knowitall.tool.parse.graph.DependencyGraph

/** An enumerator for parser options */
object Parser extends Enumeration {
  type ParserEnum = Value

  val Deserialize = Value("Deserialize")
  val Stanford = Value("Stanford")
  val MaltL = Value("Malt (Linear)")
  val MaltPoly = Value("Malt (Poly)")

  def default = MaltL

  def load(parserType: ParserEnum): (ParserEnum, DependencyParser) = parserType match {
    case Parser.Stanford => (parserType, new edu.knowitall.tool.parse.StanfordParser)
    case Parser.MaltL => (parserType, new MaltParser())
    case Parser.MaltPoly => (parserType, new MaltParser(modelUrl = new java.io.File("engmalt.poly-1.7.mco").toURI.toURL))
    case Parser.Deserialize => (parserType, new DependencyParser() {
      override def dependencies(input: String) = Dependencies.deserialize(input)
      override def dependencyGraph(input: String) = DependencyGraph.deserialize(input)
    })
  }
}
