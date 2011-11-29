package edu.washington.cs.knowitall
package pattern
package lda

import common.Random

import scala.io.Source
import scala.collection.mutable

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.PrintWriter

object CreateTestSet {
  val testSize = 100

  final case class Extraction(val rel: String, val arg1: String, val arg2: String) {
    override def toString = "("+arg1+", "+rel+", "+arg2+")"
  }

  def main(args: Array[String]) {
    val sourceFilePath = args(0)
    val testFilePath = args(1)
    val trainFilePath = args(2)

    println("opening output files")
    val testWriter = new PrintWriter(new File(testFilePath))
    val trainWriter = new PrintWriter(new File(trainFilePath))
    println("building a test set of 100 random seed extractions.")
    var testRows = 0
    var trainRows = 0
    try {
      var extractions = Set[Extraction]()

      for (line <- Source.fromFile(sourceFilePath).getLines) {
        val Array(rel, arg1, arg2, _*) = line.split("\t")
        extractions += Extraction(rel, arg1, arg2)
      }

      val rand = new scala.util.Random
      val testExtractions = Random.choose(extractions, testSize, rand)

      for (line <- Source.fromFile(sourceFilePath).getLines) {
        val Array(rel, arg1, arg2, pattern, _*) = line.split("\t")
        val extr = Extraction(rel, arg1, arg2)

        if (testExtractions.contains(extr)) {
          testWriter.println(Iterable(rel, arg1, arg2, pattern).mkString("\t"))
          testRows += 1
        }
        else {
          trainWriter.println(Iterable(rel, arg1, arg2, pattern).mkString("\t"))
          trainRows += 1
        }
      }

      println("10 random test seed extractions.")
      for (seed <- extractions.take(10)) {
        println("\t" + seed)
      }
    }
    finally {
      trainWriter.close
      testWriter.close
    }

    println(testRows + " test rows")
    println(trainRows + " test rows")
    println("done.")
  }
}
