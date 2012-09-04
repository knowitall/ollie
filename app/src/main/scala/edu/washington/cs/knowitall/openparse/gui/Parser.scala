package edu.washington.cs.knowitall.openparse.gui

import edu.washington.cs.knowitall.tool.parse.DependencyParser
import edu.washington.cs.knowitall.tool.parse.MaltParser

/** An enumerator for parser options */
object Parser extends Enumeration {
  type Parser = Value

  val Stanford = Value("Stanford")
  val MaltL = Value("Malt (Linear)")

  def default = MaltL

  def load(parserType: Parser): DependencyParser = parserType match {
    case Parser.Stanford => new edu.washington.cs.knowitall.tool.parse.StanfordParser
    case Parser.MaltL => new MaltParser()
  }
}