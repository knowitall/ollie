package edu.washington.cs.knowitall.openparse.gui

import edu.washington.cs.knowitall.tool.parse.DependencyParser
import edu.washington.cs.knowitall.tool.parse.MaltParser
import edu.washington.cs.knowitall.tool.parse.BllipParser
import edu.washington.cs.knowitall.tool.parse.graph.Dependencies
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

/** An enumerator for parser options */
object Parser extends Enumeration {
  type ParserEnum = Value

  val Deserialize = Value("Deserialize")
  val Stanford = Value("Stanford")
  val MaltL = Value("Malt (Linear)")
  val MaltPoly = Value("Malt (Poly)")
  val Bllip = Value("Bllip")

  def default = MaltL

  def load(parserType: ParserEnum): (ParserEnum, DependencyParser) = parserType match {
    case Parser.Stanford => (parserType, new edu.washington.cs.knowitall.tool.parse.StanfordParser)
    case Parser.MaltL => (parserType, new MaltParser())
    case Parser.MaltPoly => (parserType, new MaltParser(modelUrl = new java.io.File("engmalt.poly-1.7.mco").toURI.toURL))
    case Parser.Bllip => (parserType, new BllipParser())
    case Parser.Deserialize => (parserType, new DependencyParser() {
      override def dependencies(input: String) = Dependencies.deserialize(input)
      override def dependencyGraph(input: String) = DependencyGraph.deserialize(input)
    })
  }
}
