package edu.knowitall.ollie.output

import edu.knowitall.ollie.OllieExtractionInstance
import edu.knowitall.openparse.extract.Extraction
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.ollie.ExtractionPart
import edu.knowitall.tool.segment.Segment
import java.io.PrintWriter

class BratOutput(extractor: String => Iterable[OllieExtractionInstance]) {
  def process(sentences: Iterable[Segment], writer: PrintWriter) = {
    val document = new Document()
    for {
      Segment(text, offset) <- sentences 
      inst <- extractor(text)
      entry <- document.annotations(inst, offset)
    } {
      writer.println(entry)
    }
  }

  class Document {
    var entityIndex = 0
    var relationIndex = 0

    def annotations(inst: OllieExtractionInstance, sentenceCharacterOffset: Int) = {
      def partToAnnotation(inst: OllieExtractionInstance, part: Extraction.Part, partName: String) = {
        val tokens = inst.sentence.nodes.toList.slice(part.span.start, part.span.end)
        val charInterval = Interval.open(tokens.head.offset, tokens.last.offsets.end)
        partName + " " + (sentenceCharacterOffset + charInterval.start) + " " + (sentenceCharacterOffset + charInterval.end) + "\t" + inst.sentence.text.substring(charInterval.start, charInterval.end)
      }

      case class LabelledEntry(label: String, entry: String)
      def label(identifier: Char, index: Int, entry: String) = LabelledEntry(identifier.toString + index, entry)

      val entries = {
        val arguments = List(inst.extr.arg1, inst.extr.arg2) map { arg =>
          val labelled = label('T', entityIndex, partToAnnotation(inst, arg, "Argument"))
          entityIndex += 1
          labelled
        }
        val relation = {
          val labelled = label('T', entityIndex, partToAnnotation(inst, inst.extr.rel, "Relation"))
          entityIndex += 1
          labelled
        }

        val entities = relation :: arguments

        val relations = arguments zip List("Arg1", "Arg2") map {
          case (entry, edge) =>
            val labelled = label('R', relationIndex, edge + "-of Arg1:" + relation.label + " Arg2:" + entry.label)
            relationIndex += 1
            labelled
        }

        entities ::: relations
      }

      entries map {
        case LabelledEntry(label, entry) => label + "\t" + entry
      }
    }
  }
}
