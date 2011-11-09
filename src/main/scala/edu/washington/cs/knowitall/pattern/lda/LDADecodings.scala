package edu.washington.cs.knowitall.pattern.lda

import java.io._
import scala.io._

object Decode {
  def readRelFile(source: Source): Array[Array[Int]] =
    (for (line <- source.getLines if !line.trim.isEmpty) yield {
      line.split("\\s+").map(_.toInt - 1) // index patterns from 0
    }).toArray

  def readDecoding(source: Source): Map[Int, String] =
    (for (line <- source.getLines) yield {
      val Array(id, string) = line.split("\t")
      (id.toInt, string)
    }).toMap
}
