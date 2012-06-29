package edu.washington.cs.knowitall.ollie.confidence

import java.util.regex.Pattern

import scala.Array.canBuildFrom
import scala.collection.JavaConversions.setAsJavaSet
import scala.collection.immutable.SortedMap

import edu.washington.cs.knowitall.ollie.Ollie
import edu.washington.cs.knowitall.ollie.OllieExtractionInstance
import edu.washington.cs.knowitall.openparse.extract.Extraction.Part
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer.instance
import edu.washington.cs.knowitall.tool.stem.Stemmer
import scalaz.Scalaz._

object OllieExtractionFeatures {
  implicit def boolToDouble(bool: Boolean) = if (bool) 1.0 else 0.0

  val weirdPunct = Pattern.compile(".*[:!@#$%^&*{};`<>]+.*")
  val hasQuestionMark = Pattern.compile(".*\\?.*")
  val prepTag = Pattern.compile("IN|TO|WP")
  val ingStart = Pattern.compile("^[a-zA-Z]+ing.*")
  val verbStart = Pattern.compile("VBZ?")

  def sentenceHasQuestionMark(inst: OllieExtractionInstance): Double = {

    if (hasQuestionMark.matcher(inst.extr.text).matches()) 1.0 else 0.0
  }

  // does the sentence contain an EnabledBy prefix token, AND have an empty EnabledBy slot??
  def sentHasPrefix(inst: OllieExtractionInstance): Double = {
    inst.extr.enabler match {
      case Some(_) =>
        inst.sent.nodes.iterator.map(_.text).exists(Ollie.enablerPrefixes.contains(_))
      case None => true
    }
  }

  // is there a verb at the start of the sentence, or immediately after a comma?
  def verbStartsPhrase(inst: OllieExtractionInstance): Double = {
    import scalaz._
    import Scalaz._

    val postagZipper = inst.sent.nodes.iterator.map(_.postag).toList.toZipper

    // Assume the sentence starts with a comma to simplify the computation:
    // pair every token with the one before it, pairing the start with a comma.
    val boolean = postagZipper.map(_.positions.toStream.exists { zipper =>
      zipper.previous match {
        case None => verbStart.matcher(zipper.focus).matches()
        case Some(z) if z.focus == "," => verbStart.matcher(zipper.focus).matches()
        case _ => false
      }
    }).getOrElse(false)

    boolean
  }

  // does arg2 contain an infinitive?
  def arg2ContainsInfinitive(inst: OllieExtractionInstance): Double = {
    val postags = inst.extr.arg2.nodes.iterator.map(_.postag).toStream

    import scalaz._
    import Scalaz._

    val boolean = postags.toZipper.map(_.positions.toStream.exists { zipper =>
      zipper.previous match {
        case Some(prev) => prev.focus == "TO" && zipper.focus.startsWith("VB")
        case None => false
      }
    }).getOrElse(false)

    boolean
  }

  // is rel a contiguous set of tokens from the sentence?
  def relIsContiguous(inst: OllieExtractionInstance): Double = {
    (inst.sent.nodes.iterator.map(_.text).mkString(" ")) contains inst.extr.rel.text
  }

  def commVerbBeforeArg1(inst: OllieExtractionInstance): Double = {
    val beforeArg1 = inst.sent.nodes.take(inst.extr.arg1.span.start)

    beforeArg1.exists { node =>
      Ollie.communicationWords.contains(node.lemma) ||
      Ollie.cognitiveWords.contains(node.lemma)
    }
  }

  // is there a prep right before arg1?
  def prepRightBeforeArg1(inst: OllieExtractionInstance): Double = {
    val arg1Span = inst.extr.arg1.span
    val rightBeforeArg1 = inst.sent.nodes.find(node => node.indices < arg1Span && node.indices.borders(arg1Span))

    rightBeforeArg1 match {
      case Some(node) => prepTag.matcher(node.postag).matches()
      case None => false
    }
  }

  // does rel start with "be"?
  def relStartsWithBe(inst: OllieExtractionInstance): Double = {
    inst.extr.rel.nodes.headOption match {
      case Some(node) => node.text startsWith "be"
      case None => false
    }
  }

  // is there a prep right after arg2? (IN or TO pos tag)
  def prepRightAfterArg2(inst: OllieExtractionInstance): Double = {
    val arg2Span = inst.extr.arg2.span

    val rightAfterArg2 = inst.sent.nodes.find { node =>
      node.indices > arg2Span && (node.indices borders arg2Span)
    }

    rightAfterArg2 match {
      case Some(node) => prepTag.matcher(node.postag).matches
      case None => false
    }
  }

  def argIsProper(part: Part): Double = {
    part.nodes.forall(_.isProperNoun)
  }

  def arg1IsProper(inst: OllieExtractionInstance): Double = { argIsProper(inst.extr.arg1) }

  def arg2IsProper(inst: OllieExtractionInstance): Double = { argIsProper(inst.extr.arg2) }

  def sentStartsWithExtr(inst: OllieExtractionInstance): Double = {
    inst.extr.arg1.span.start == 0 || inst.extr.rel.span.start == 0 || inst.extr.arg2.span.start == 0
  }

  def relContainsPostag(pos: String)(inst: OllieExtractionInstance): Double = {
    inst.extr.rel.nodes.iterator.map(_.postag).exists(_.equals(pos))
  }

  def extrHasEnabledBy(inst: OllieExtractionInstance): Double = {
    inst.extr.enabler.isDefined
  }

  def extrHasAttribution(inst: OllieExtractionInstance): Double = {
    inst.extr.attribution.isDefined
  }

  def openParseConfidence(inst: OllieExtractionInstance): Double = {
    scala.math.min(1.0, inst.extr.confidence)
  }

  def arg2ComesBeforeArg1(inst: OllieExtractionInstance): Double =
    inst.extr.arg2.span < inst.extr.arg1.span && !(inst.extr.arg1.span intersects inst.extr.arg2.span)

  def relCommunicationVerb(inst: OllieExtractionInstance): Double = {
    inst.extr.rel.nodes.exists{node =>
      node.postag.startsWith("VB") && {
        val stemmed = implicitly[Stemmer].stem(node.text)
        Ollie.communicationWords.contains(stemmed)
      }
    }
  }

  def argsStartEndWithNoun(inst: OllieExtractionInstance): Double = {
    val okStarts = Set("PR", "NN", "DT", "CD", "JJ")
    val okEnds = Set("NN", "CD", "JJ")

    val boolean = (for {
      arg1HeadTag <- inst.extr.arg1.nodes.headOption.map(_.postag)
      arg2HeadTag <- inst.extr.arg2.nodes.headOption.map(_.postag)
      arg1LastTag <- inst.extr.arg1.nodes.lastOption.map(_.postag)
      arg2LastTag <- inst.extr.arg2.nodes.lastOption.map(_.postag)
    } yield {
      okStarts(arg1HeadTag) && okStarts(arg2HeadTag) &&
      okEnds(arg1LastTag) && okEnds(arg2LastTag)
    }).getOrElse(false)

    boolean
  }

  def ifBeforeArg1(inst: OllieExtractionInstance): Double = {
    val nodesSeq = inst.sent.nodes.take(inst.extr.arg1.span.start - 1)
    nodesSeq.exists(_.text equalsIgnoreCase "if")
  }

  def arg1ContainsPronoun(inst: OllieExtractionInstance): Double = {
    inst.extr.arg1.nodes.exists(_.equals("PRP"))
  }

  def relEndsWithOf(inst: OllieExtractionInstance): Double = {
    // of will be an edge in the collapsed graph so it's not represented by a node
    val boolean = inst.extr.rel.text.drop(inst.extr.rel.text.lastIndexOf(" ") + 1) == "of"

    boolean
  }

  def verbAfterArg2(inst: OllieExtractionInstance): Double = {
    inst.sent.nodes.drop(inst.extr.arg2.span.end).exists { node =>
      node.isVerb
    }
  }

  def thatBeforeRel(inst: OllieExtractionInstance): Double = {
    inst.sent.nodes.take(inst.extr.rel.span.start).exists { node =>
      node.text equalsIgnoreCase "that"
    }
  }

  val capsPattern = Pattern.compile("[^A-Z]")
  val junkPattern = Pattern.compile("\\p{Punct}+|\\s+")
  val numbersPattern = Pattern.compile("[0-9]+")
  val neededChars = Pattern.compile("[^AEIOUYaeiouy0-9]")
  val infinitive = Pattern.compile("(to|TO)\\s+[a-zA-Z]+\\s?[a-zA-Z]*")
  val badArg1Pos = Set("CC", "DT")
  def arg1CleanupFeature(inst: OllieExtractionInstance): Double = {
    val arg1Text = inst.extr.arg1.text

    // are there more than 5 caps in arg1?
    if (capsPattern.matcher(arg1Text).replaceAll("").length() > 5) {
      1.0
    } // are there not enough good characters?
    else if (neededChars.matcher(arg1Text).replaceAll("").length() == 0) {
      1.0
    } // is arg1 just a conjunction or adjective?
    else if (inst.extr.arg1.nodes.size == 1 && badArg1Pos.contains(inst.extr.arg1.nodes.head)) {
      1.0
    } // does arg1 end with a conjunction or adjective?
    else if (badArg1Pos.contains(inst.extr.arg1.nodes.last.postag)) {
      1.0
    } // is rel really long?
    else if (inst.extr.rel.text.split(" ").length > 11) {
      1.0
    } else if (numbersPattern.matcher(junkPattern.matcher(inst.extr.arg1.text).replaceAll("")).matches()) {
      1.0
    } else {
      0.0
    }
  }

  val capStart = Pattern.compile("[^\\s]*(^|\\s)([A-Z][a-z]*).*")
  def relCleanupFeature(inst: OllieExtractionInstance): Double = {
    // are there more than 5 caps in arg1?
    if (capsPattern.matcher(inst.extr.rel.text).replaceAll("").length() > 5)
      true
    // are there not enough good characters?
    else if (inst.extr.rel.text.matches("\\s*"))
      true
    else {
      val capMatcher = capStart.matcher(inst.extr.rel.text)
      capMatcher.find() match {
        case true => {
          val word = capMatcher.group(2)
          inst.sent.nodes.head.text.equalsIgnoreCase(word)
        }
        case false => false
      }
    }
  }

  def hypWordsNearRel(inst: OllieExtractionInstance): Double = {
    val hypWords = Set("can", "would", "could", "might", "who")
    val relTextWords = inst.extr.rel.text.split(" ").toSet
    inst.sent.nodes.exists { node =>
      (hypWords contains node.text.toLowerCase) &&
        !(relTextWords contains node.text.toLowerCase)
    } match {
      case true => 1.0
      case false => 0.0
    }
  }

  /*
   * Tries to capture vacuous extractions, like
   * Bob; was for; example
   * by specifying pairs like ("for","example") to look for adjacent to each other between rel-arg2
   */
  val vacuoi = Set(("this", "way"), ("went", "public"), ("for", "example"), ("with", "eye"), ("in", "fact"), ("for", "use"), ("for", "us"), ("were", "able"), ("in", "part"), ("be", "part"), ("is", "part"))
  def vacuousRelArg2(inst: OllieExtractionInstance): Double = {
    val relEnd = inst.extr.rel.text.drop(inst.extr.rel.text.lastIndexOf(" ") + 1)
    val arg2Strings = inst.extr.arg2.text.split(" ").map(_.toLowerCase())
    if (arg2Strings.size > 2)
      false
    else {
      vacuoi.exists(p => relEnd.startsWith(p._1) && arg2Strings.contains(p._2.toLowerCase())) match {
        case true => 1.0
        case false => 0.0
      }
    }
  }

  // The full set of all implemented features in this format is commented out below this method
  def getFeatures(): SortedMap[String, OllieExtractionInstance => Double] = {
    SortedMap(
      "Arg1 comes after arg2" -> arg2ComesBeforeArg1,
      "Args both start and end with nouns" -> argsStartEndWithNoun,
      "Extr has attribution?" -> extrHasAttribution,
      "Extr has enabledBy?" -> extrHasEnabledBy,
      "If before arg1 and no enabledBy?" -> ifBeforeArg1,
      "Hyp words near rel" -> hypWordsNearRel,
      "OpenParse Confidence" -> openParseConfidence,
      "Prep right after arg2?" -> prepRightAfterArg2,
      "Prep right before arg1?" -> prepRightBeforeArg1,
      "Rel contains VBD" -> (relContainsPostag("VBD")),
      "Rel contains VBG" -> (relContainsPostag("VBG")),
      "Rel ends with of" -> relEndsWithOf,
      "Rel contains VBP" -> (relContainsPostag("VBP")),
      "Rel head is a communication verb?" -> relCommunicationVerb,
      "Rel starts with \"be\"?" -> relStartsWithBe,
      "Sentence has question mark?" -> sentenceHasQuestionMark,
      "Sentence starts with extr?" -> sentStartsWithExtr,
      "That before rel" -> thatBeforeRel,
      "Verb after arg2?" -> verbAfterArg2,
      "Arg1 Cleanup feature" -> arg1CleanupFeature,
      "Vacuous rel+arg2" -> vacuousRelArg2)

    // known good (decent at least) features:
    /*
    SortedMap(
      "Arg1 comes after arg2" -> arg2ComesBeforeArg1,
      "Arg1 has more than one B-NP?" -> arg1MoreThanOneNP,
      //"Arg1 is proper?" -> arg1IsProper,
      //"arg1 overlaps with other extr arg2?" -> arg1OverlapsRvArg2,
      //"Arg2 is proper?" -> arg2IsProper,
      "Args both start and end with nouns" -> argsStartEndWithNoun,
      //"Comm or cog verb before arg1?" -> commVerbBeforeArg1,
      "Does an RV extraction overlap rel?" -> rvExtrOverlapsRel,
      //"Does arg2 contain a comma?" -> arg2HasComma,
      //"Does arg2 contain an infinitive?" -> arg2ContainsInfinitive,
      "Extr has attribution?" -> extrHasAttribution,
      "Extr has enabledBy?" -> extrHasEnabledBy,
      //"F1" -> f1,
      //"F2" -> f2,
      "If before arg1 and no enabledBy?" -> ifBeforeArg1,
      "Hyp words near rel" -> hypWordsNearRel,
      "OpenParse Confidence" -> openParseConfidence,
      //"OpenParse only?" -> openParseOnly,
      "Prep in rel?" -> prepInRel,
      "Prep right after arg2?" -> prepRightAfterArg2,
      "Prep right before arg1?" -> prepRightBeforeArg1,
      //"Rel contains VB" -> relContainsPos("VB"),
      "Rel contains VBD" -> relContainsPos("VBD"),
      "Rel contains VBG" -> relContainsPos("VBG"),
      "Rel ends with of" -> relEndsWithOf,
      //"Rel contains VBN" -> relContainsPos("VBN"),
      "Rel contains VBP" -> relContainsPos("VBP"),
      //"Rel contains VBZ" -> relContainsPos("VBZ"),
      "Rel head is a communication verb?" -> relCommunicationVerb,
      //"Rel is contiguous?" -> relIsContiguous,
      "Rel starts with \"be\"?" -> relStartsWithBe,
      "RV extraction within arg2?" -> rvExtrWithinArg2,
      //"Sentence has enabledByPrefix but vacant slot" -> sentHasPrefix,
      "Sentence has question mark?" -> sentenceHasQuestionMark,
      "Sentence starts with extr?" -> sentStartsWithExtr,
      "That before rel" -> thatBeforeRel,
      "Verb after arg2?" -> verbAfterArg2,
      //"Verb starts phrase?" -> verbStartsPhrase,
      "Arg1 Cleanup feature" -> arg1CleanupFeature,
      //"Rel Cleanup feature" -> relCleanupFeature,
      "Vacuous rel+arg2" -> vacuousRelArg2
    )
 */
  }
}
