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
import edu.washington.cs.knowitall.tool.postag.Postagger
import scala.util.matching.Regex

object OllieFeatureSet extends FeatureSet(OllieFeatures.getFeatures)

/** Features defined for OllieExtractionInstances */
object OllieFeatures {
  implicit def boolToDouble(bool: Boolean) = if (bool) 1.0 else 0.0

  val weirdPunct = Pattern.compile(".*[:!@#$%^&*{};`<>]+.*")
  val prepTag = Pattern.compile("IN|TO|WP")
  val ingStart = Pattern.compile("^[a-zA-Z]+ing.*")
  val verbStart = Pattern.compile("VB*")
  val relationVerb = Pattern.compile("VB|VBD|VBZ|VBN|VBP|MD")

  object nonContinuousRel extends Feature("non-contiguous rel") {
    val trainingPrep = new Regex(" (?:" + Postagger.prepositions.mkString("|") + ")$")
    override def apply(inst: OllieExtractionInstance): Double = {
      val trimmed = trainingPrep.replaceAllIn(inst.extr.rel.text, "")
      inst.sent.text.contains(trimmed)
    }
  }

  class gapInRel(length: Int) extends Feature("gap of " + length + " in rel") {
    override def apply(inst: OllieExtractionInstance): Double = {
      inst.extr.nodes.toSeq.sliding(2).exists { case Seq(x, y) =>
        x.indices.distance(y.indices) > 10
      }
    }
  }

  object sentenceHasQuestionMark extends Feature("sentence has question mark") {
    val hasQuestionMark = Pattern.compile(".*\\?.*")
    override def apply(inst: OllieExtractionInstance): Double = {
      if (hasQuestionMark.matcher(inst.extr.text).matches()) 1.0 else 0.0
    }
  }

  // is there a verb at the start of the sentence, or immediately after a comma?
  object imperative extends Feature("sentence is imperative") {
    override def apply(inst: OllieExtractionInstance): Double = {
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
  }

  // does arg2 contain an infinitive?
  object arg2ContainsInfinitive extends Feature("arg2 contains infinitive") {
    override def apply(inst: OllieExtractionInstance): Double = {
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
  }

  // is rel a contiguous set of tokens from the sentence?
  object relIsContiguous extends Feature("rel is contiguous") {
    override def apply(inst: OllieExtractionInstance): Double = {
      (inst.sent.nodes.iterator.map(_.text).mkString(" ")) contains inst.extr.rel.text
    }
  }

  // is there a prep right before arg1?
  object prepRightBeforeArg2 extends Feature("prep right before arg2") {
    override def apply(inst: OllieExtractionInstance): Double = {
      val arg1Span = inst.extr.arg1.span
      val rightBeforeArg1 = inst.sent.nodes.find(node => node.indices < arg1Span && node.indices.borders(arg1Span))

      rightBeforeArg1 match {
        case Some(node) => prepTag.matcher(node.postag).matches()
        case None => false
      }
    }
  }

  // does rel start with "be"?
  object relStartsWithBe extends Feature("rel starts with be") {
    override def apply(inst: OllieExtractionInstance): Double = {
      inst.extr.rel.nodes.headOption match {
        case Some(node) => node.text startsWith "be"
        case None => false
      }
    }
  }

  // is there a prep right after arg2? (IN or TO pos tag)
  object prepRightAfterArg2 extends Feature("prep right after arg2") {
    def apply(inst: OllieExtractionInstance): Double = {
      val arg2Span = inst.extr.arg2.span

      val rightAfterArg2 = inst.sent.nodes.find { node =>
        node.indices > arg2Span && (node.indices borders arg2Span)
      }

      rightAfterArg2 match {
        case Some(node) => prepTag.matcher(node.postag).matches
        case None => false
      }
    }
  }

  class ArgIsProper(getPart: OllieExtractionInstance=>Part, partName: String) extends Feature(partName + " is proper") {
    override def apply(inst: OllieExtractionInstance): Double = {
      getPart(inst).nodes.forall(_.isProperNoun)
    }
  }

  object sentStartsWithExtr extends Feature("sentence starts with extraction") {
    override def apply(inst: OllieExtractionInstance): Double = {
      inst.extr.span.start == 0
    }
  }

  object extrSpansSent extends Feature("extraction spans sentence") {
    override def apply(inst: OllieExtractionInstance): Double = {
      inst.extr.span == inst.sent.interval
    }
  }

  object sentEndsWithExtr extends Feature("sentence ends with extraction") {
    override def apply(inst: OllieExtractionInstance): Double = {
      inst.extr.span.end == inst.sent.interval.end
    }
  }

  object sentBeginsWithArg1 extends Feature("sentence begins with arg1") {
    override def apply(inst: OllieExtractionInstance): Double = {
      inst.extr.arg1.span.start == 0
    }
  }

  object sentEndsWithArg2 extends Feature("sentence ends with arg2") {
    override def apply(inst: OllieExtractionInstance): Double = {
      inst.extr.arg2.span.end == inst.sent.length
    }
  }

  object openParseConfidence extends Feature("openparse confidence") {
    override def apply(inst: OllieExtractionInstance): Double = {
      scala.math.min(1.0, inst.extr.openparseConfidence)
    }
  }

  object arg2BeforeArg1 extends Feature("arg2 before arg1") {
    override def apply(inst: OllieExtractionInstance): Double =
      inst.extr.arg2.span < inst.extr.arg1.span && !(inst.extr.arg1.span intersects inst.extr.arg2.span)
  }

  object arg2BeforeRel extends Feature("arg2 before rel") {
    override def apply(inst: OllieExtractionInstance): Double =
      inst.extr.arg2.span < inst.extr.rel.span && !(inst.extr.rel.span intersects inst.extr.arg2.span)
  }

  object argsStartEndWithNoun extends Feature("args start and end with noun") {
    override def apply(inst: OllieExtractionInstance): Double = {
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
  }

  object ifRightBeforeArg1 extends Feature("if right before arg1") {
    override def apply(inst: OllieExtractionInstance): Double = {
      val nodesSeq = inst.sent.nodes.take(inst.extr.arg1.span.start - 1)
      nodesSeq.lastOption.exists(_.text equalsIgnoreCase "if")
    }
  }

  object arg1ContainsPronoun extends Feature("arg1 contains pronoun") {
    override def apply(inst: OllieExtractionInstance): Double = {
      inst.extr.arg1.nodes.exists(_.isPronoun)
    }
  }

  object arg2ContainsPronoun extends Feature("arg2 contains pronoun") {
    override def apply(inst: OllieExtractionInstance): Double = {
      inst.extr.arg2.nodes.exists(_.isPronoun)
    }
  }

  object relEndsWithOf extends Feature("rel ends with of") {
    override def apply(inst: OllieExtractionInstance): Double = {
      // of will be an edge in the collapsed graph so it's not represented by a node
      val boolean = inst.extr.rel.text.drop(inst.extr.rel.text.lastIndexOf(" ") + 1) == "of"

      boolean
    }
  }

  object relContainsVerb extends Feature("rel contains verb") {
    override def apply(inst: OllieExtractionInstance): Double = {
      val boolean = inst.extr.rel.nodes exists (_.isVerb)
      boolean
    }
  }

  object relContainsVBG extends Feature("rel contains gerund") {
    override def apply(inst: OllieExtractionInstance): Double = {
      val boolean = inst.extr.rel.nodes exists (_.isVerbGerund)
      boolean
    }
  }

  class BadCharacters(getPart: OllieExtractionInstance=>Part, partName: String) extends Feature(partName + " bad characters") {
    val notCapsPattern = Pattern.compile("[^A-Z]")
    val weirdChars = Pattern.compile("[^AEIOUYaeiouy0-9]")
    override def apply(inst: OllieExtractionInstance): Double = {
      // are there more than 5 caps in arg1?
      if (notCapsPattern.matcher(getPart(inst).text).replaceAll("").length() > 5)
        1.0
      // are there not enough good characters?
      else if (weirdChars.matcher(getPart(inst).text).replaceAll("").length() < 2)
        1.0
      else
        0.0
    }
  }

  object longRelation extends Feature("long relation") {
    override def apply(inst: OllieExtractionInstance) = {
      val boolean = inst.extr.rel.text.split(" ").length > 10

      boolean
    }
  }

  object hypWordsInRel extends Feature("hyp words in rel") {
    override def apply(inst: OllieExtractionInstance): Double = {
      val hypWords = Set("can", "would", "could", "might")
      val relTextWords = inst.extr.rel.text.split(" ").toSet
      inst.sent.nodes.exists { node =>
        (hypWords contains node.text.toLowerCase) &&
          !(relTextWords contains node.text.toLowerCase)
      } match {
        case true => 1.0
        case false => 0.0
      }
    }
  }

  /*
   * Tries to capture vacuous extractions, like
   * Bob; was for; example
   * by specifying pairs like ("for","example") to look for adjacent to each other between rel-arg2
   */
  object vacuousExtraction extends Feature("vacuous extraction") {
    val vacuoi = Set(("this", "way"), ("went", "public"), ("for", "example"), ("with", "eye"), ("in", "fact"), ("for", "use"), ("for", "us"), ("were", "able"), ("in", "part"), ("be", "part"), ("is", "part"))
    def apply(inst: OllieExtractionInstance): Double = {
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
  }

  // Is there a preposition in the span of arg2?
  object prepInArg2 extends Feature("prep in arg2") {
    override def apply(inst: OllieExtractionInstance): Double = {
      inst.extr.arg2.nodes.exists(node => prepTag.matcher(node.postag).matches)
    }
  }

  // does the pattern "... NN* ... VB* ... NN* ... " exist in arg1 or arg2?
  class NounVerbNounInArg(getPart: OllieExtractionInstance=>Part, partName: String) extends Feature("noun-verb-noun in " + partName) {
    override def apply(inst: OllieExtractionInstance): Double = {
      val tokenSet = getPart(inst).nodes
      val tokens = tokenSet.iterator.toSeq
      val firstNN = tokens.indexWhere(_.isNoun)
      val firstVB = tokens.drop(firstNN).indexWhere(_.isVerb)
      val secondNN = tokens.drop(firstNN + firstVB).indexWhere(_.isNoun)

      def tokensExist = !Seq(firstNN, firstVB, secondNN).exists(_ == -1)
      def correctIndices = (firstNN < (firstNN + firstVB) < (firstNN + firstVB + secondNN))
      tokensExist && correctIndices
    }
  }

  class ArgBordersComma(getPart: OllieExtractionInstance=>Part, partName: String) extends Feature(partName + " borders comma") {
    override def apply(inst: OllieExtractionInstance): Double = {
      val neighbors = inst.sent.nodes.filter(_.indices borders getPart(inst).span)
      neighbors exists (_.text == ",")
    }
  }

  // features being used
  val features: List[Feature] = List(
    sentenceHasQuestionMark,
    imperative,
    arg2ContainsInfinitive,
    relIsContiguous,
    prepRightBeforeArg2,
    relStartsWithBe,
    prepRightAfterArg2,
    new ArgIsProper(_.extr.arg1, "arg1"),
    new ArgIsProper(_.extr.arg2, "arg2"),
    sentStartsWithExtr,
    sentBeginsWithArg1,
    sentBeginsWithArg1,
    sentEndsWithArg2,
    openParseConfidence,
    arg2BeforeArg1,
    arg2BeforeRel,
    argsStartEndWithNoun,
    ifRightBeforeArg1,
    arg1ContainsPronoun,
    arg2ContainsPronoun,
    relEndsWithOf,
    relContainsVerb,
    relContainsVBG,
    new BadCharacters(_.extr.arg1, "arg1"),
    new BadCharacters(_.extr.rel, "rel"),
    new BadCharacters(_.extr.arg2, "arg2"),
    longRelation,
    hypWordsInRel,
    vacuousExtraction,
    prepInArg2,
    new NounVerbNounInArg(_.extr.arg1, "arg1"),
    new NounVerbNounInArg(_.extr.arg2, "arg2"),
    new ArgBordersComma(_.extr.arg1, "arg1"),
    new ArgBordersComma(_.extr.arg2, "arg2"),
    new gapInRel(10),
    nonContinuousRel)

  def getFeatures(): SortedMap[String, OllieExtractionInstance => Double] = {
    (for (f <- features) yield (f.name -> (f.apply _)))(scala.collection.breakOut)
  }
}
