package edu.washington.cs.knowitall
package pattern.lda

import scala.io.Source
import scala.collection.mutable
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.PrintWriter
import tool.parse.pattern.DependencyPattern

object CreateLDAFiles {
  def main(args: Array[String]) {
    val sourcePath = args(0)
    val dest = args(1)
   
    // encodings
    val wordEncodingFile = new PrintWriter(new BufferedWriter(new FileWriter(dest + "/word_encoding.txt")))
    val patternEncodingFile = new PrintWriter(new BufferedWriter(new FileWriter(dest + "/pattern_encoding.txt")))
    val relationEncodingFile = new PrintWriter(new BufferedWriter(new FileWriter(dest + "/relation_encoding.txt")))
    
    // relationships
    val relPatternFile = new PrintWriter(new BufferedWriter(new FileWriter(dest + "/rel_pattern.txt")))
    val relArg1File = new PrintWriter(new BufferedWriter(new FileWriter(dest + "/rel_arg1.txt")))
    val relArg2File = new PrintWriter(new BufferedWriter(new FileWriter(dest + "/rel_arg2.txt")))
    /*
    val relSlot1File = new PrintWriter(new BufferedWriter(new FileWriter(dest + "/pattern_slot1.txt")))
    val relSlot2File = new PrintWriter(new BufferedWriter(new FileWriter(dest + "/pattern_slot2.txt")))
    */
    
    var source: Source = null
    
    println("creating word encodings...")
    source = Source.fromFile(sourcePath)
    val wordEncoding = createWordEncoding(source)
    source.close
    
    println("creating pattern encodings...")
    source = Source.fromFile(sourcePath)
    val patternEncoding = createPatternEncoding(source)
    source.close
    
    println("writing word encodings...")
    writeEncoding(wordEncoding, wordEncodingFile)
    println("writing pattern encodings...")
    writeEncoding(patternEncoding, patternEncodingFile)
    
    println("writing rel patterns")
    source = Source.fromFile(sourcePath)
    writeRelPatternFile(wordEncoding, patternEncoding, source, relPatternFile, relationEncodingFile)
    source.close
    
    println("writing rel arg1s")
    source = Source.fromFile(sourcePath)
    writeRelWordFile(wordEncoding, patternEncoding, 1, source, relArg1File)
    source.close
    
    source = Source.fromFile(sourcePath)
    writeRelWordFile(wordEncoding, patternEncoding, 2, source, relArg2File)
    source.close
    
    /*
    println("writing rel arg2s")
    source = Source.fromFile(sourcePath)
    writePatternWordFile(wordEncoding, patternEncoding, 4, source, relSlot1File)
    source.close
    
    println("writing rel slot1s")
    source = Source.fromFile(sourcePath)
    writePatternWordFile(wordEncoding, patternEncoding, 5, source, relSlot2File)
    source.close
    */
    
    println("writing rel slot2s")
    wordEncodingFile.close
    patternEncodingFile.close
    relationEncodingFile.close
    relPatternFile.close
    relArg1File.close
    relArg2File.close
    /*
    relSlot1File.close
    relSlot2File.close
    */
  }
  
  def createWordEncoding(source: Source) = {
    var index = 1
    var map = new mutable.HashMap[String, Int]
    
    source.getLines.foreach { line => 
      val Array(rel, arg1, arg2, pattern, slots @ _*) = line.split("\t", -1)
      val words = List() ++ rel.split("\\s+") ++ arg1.split("\\s+") ++ arg2.split("\\s+") ++ slots.flatMap(_.split("\\s+"))
      
      words.foreach { word =>
        if (!map.contains(word))
          map += word -> index
          index += 1
      }
    }
    
    map
  }
  
  def createPatternEncoding(source: Source) = {
    var index = 1
    var map = new mutable.HashMap[String, Int]
    
    source.getLines.foreach { line => 
      val Array(rel, arg1, arg2, pattern, slots @ _*) = line.split("\t", -1)
      assert(DependencyPattern.deserialize(pattern).toString != null, pattern)
      if (!map.contains(pattern)) {
        map += pattern -> index
        index += 1
      }
    }
    
    map
  }
  
  def writeEncoding(encoding: mutable.Map[String, Int], output: PrintWriter) {
    encoding.keys.toList.sorted.foreach { key =>
      output.println(encoding(key) + "\t" + key)
    }
  }
  
  def writeRelPatternFile(wordEncoding: mutable.Map[String, Int], patternEncoding: mutable.Map[String, Int], source: Source, output: PrintWriter, relOutput: PrintWriter) {
    var map = new mutable.HashMap[String, List[Int]]
    source.getLines.foreach { line => 
      val Array(rel, arg1, arg2, pattern, slots @ _*) = line.split("\t", -1)
      
      val list = map.getOrElse(rel, List.empty)
      map += rel -> (patternEncoding(pattern) :: list)
    }
    
    map.keys.toList.zipWithIndex.foreach { case (key, i) =>
      relOutput.println(i + "\t" + key)
      output.println(map(key).sortWith{ case (a, b) => a < b }.mkString(" "))
    }
  }
  
  def writeRelWordFile(wordEncoding: mutable.Map[String, Int], patternEncoding: mutable.Map[String, Int], index: Int, source: Source, output: PrintWriter) {
    var map = new mutable.HashMap[String, List[Int]]
    source.getLines.foreach { line => 
      val ar, Array(rel, arg1, arg2, pattern, slots @ _*) = line.split("\t", -1)
      
      val part = ar(index)
      part.split("\\s+").foreach { word =>
        val list = map.getOrElse(rel, List.empty)
        map += rel -> (wordEncoding(word) :: list)
      }
    }
    
    map.keys.foreach { key =>
      output.println(map(key).sortWith{ case (a, b) => a < b }.mkString(" "))
    }
  }
      
  def writePatternWordFile(wordEncoding: mutable.Map[String, Int], patternEncoding: mutable.Map[String, Int], index: Int, source: Source, output: PrintWriter) {
    var map = new mutable.HashMap[Int, List[Int]]
    source.getLines.foreach { line => 
      val ar, Array(rel, arg1, arg2, pattern, slots @ _*) = line.split("\t", -1)
      
      val part = ar(index)
      part.split("\\s+").foreach { word =>
        val encoding = patternEncoding(pattern)
        val list = map.getOrElse(encoding, List.empty)
        map += encoding -> (wordEncoding(word) :: list)
      }
    }
    
    map.keys.toList.sortWith{ case (a, b) => a < b }.foreach { key =>
      output.println(map(key).sortWith( _ < _ ).mkString(" "))
    }
  }
}
