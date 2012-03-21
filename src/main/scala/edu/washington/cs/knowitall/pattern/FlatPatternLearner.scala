package edu.washington.cs.knowitall
package parse
package pattern

import edu.washington.cs.knowitall.nlp.{ChunkedSentence, OpenNlpSentenceChunker}
import edu.washington.cs.knowitall.stemmer.MorphaStemmer
import edu.washington.cs.knowitall.Sentence
import java.io.{File, PrintWriter}

import scala.collection
import scala.collection.JavaConversions._
import scala.io.Source

object FlatPatternLearner {
  def main(args: Array[String]) {
    var source =
      if (args.length > 0) Source.fromFile(args(0), "UTF8").getLines
      else Source.stdin.getLines

    val dest =
      if (args.length > 1) new PrintWriter(new File(args(1)), "UTF8")
      else new PrintWriter(System.out)

    val chunker = new OpenNlpSentenceChunker()

    val lemmas = source.next().split("""\s+""").map(w => MorphaStemmer.instance.normalize(w))
    
    val map = new collection.mutable.HashMap[String, collection.mutable.Set[String]]() 
      with collection.mutable.MultiMap[String, String]
     source.toSet[String].foreach { line =>
      val tokens = line.split("\\s+")
      val chunked = new Sentence(chunker.chunkSentence(line), line)
      if (lemmas forall { l => chunked.lemmas().contains(l) }) {
        val pattern = findPattern(lemmas, chunked)
        map.addBinding(pattern, line)
      }
    }
    
    (map.toList sortBy { case (key, values) => values.size }).reverse.
      foreach {case (key, values) => dest.println(values.size + "\t" + key + "\t" + values.mkString("\t"))}
    
    dest.close
  }

  def findPattern(lemmas: Array[String], chunked: Sentence) = {
    def piece(lemmas: Array[String], chunked: Sentence, iterator: BufferedIterator[Int]) = {
      val index = iterator.next()
      val lemma = chunked.lemmas().get(index)
      val pos = chunked.postags().get(index)
      val chunk = chunked.chunks().get(index)

/*
      if (chunk.equals("I-NP")) {
        var containedLemmas : Set[String] = Set()
        if (lemmas.contains(lemma)) {
          containedLemmas += lemma
        }
        while (iterator.hasNext && chunked.getChunkTags().get(iterator.head).equals("I-NP")) {
          val next = chunked.getLemmas.get(iterator.next)
          if (lemmas.contains(next)) {
            containedLemmas += next
          }
        }
        
        if (!containedLemmas.isEmpty) {
          "<chunk='I-NP'>* <chunk='I-NP' & lemma='"+containedLemmas.mkString("|")+"'>+ <chunk='I-NP'>*"
        }
        else {
          "<chunk='I-NP'>+"
        }
        */
      if (chunk.equals("I-NP") || chunk.equals("B-NP")) {
        var containedLemmas : Set[String] = Set()
        if (lemmas.contains(lemma)) {
          containedLemmas += lemma
        }
        while (iterator.hasNext && chunked.chunks().get(iterator.head).equals("I-NP")) {
          val next = chunked.lemmas.get(iterator.next)
          if (lemmas.contains(next)) {
            containedLemmas += next
          }
        }
        
        if (!containedLemmas.isEmpty) {
          //"(?:<chunk='B-NP' & lemma='"+containedLemmas.mkString("|")+"')> <chunk='I-NP'>* | <chunk='B-NP'> <chunk='I-NP'>* <chunk='I-NP' & lemma='"+containedLemmas.mkString("|")+"'>+ <chunk='I-NP'>*)"
          "(npchunk with " + containedLemmas.mkString(",") + ")"
        }
        else {
          // "<chunk='B-NP'> <chunk='I-NP'>*"
          "(npchunk)"
        }
      }
      else if (lemmas.contains(lemma)) {
        "<lemma='" + lemma + "'>"
      } else if (pos.equals("DT")) {
        "<pos='" + pos + "'>?"
      } else if (pos.startsWith("VB")) {
        "<pos='VB.*'>"
      } else if (pos.startsWith("JJ")) {
        "<pos='JJ.*'>*"
      } else {
        "<pos='" + pos + "'>"
      }
    }

    val indices = lemmas.map(l => chunked.lemmas.indexOf(l))
    val (min, max) = (indices.min, indices.max)

    var pattern: List[String] = List()
    val iterator = (min to max).iterator.buffered
    while (iterator.hasNext) {
      pattern ::= piece(lemmas, chunked, iterator)
    }

    pattern.reverse.mkString(" ")
  }
}
