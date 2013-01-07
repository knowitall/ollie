package edu.washington.cs.knowitall.openparse.template

import java.io.{PrintWriter, File}

import scala.Option.option2Iterable
import scala.annotation.elidable
import scala.collection.immutable
import scala.io.Source
import scala.util.matching.Regex

import org.slf4j.LoggerFactory

import edu.washington.cs.knowitall.collection.immutable.graph.pattern.{Matcher, ConjunctiveNodeMatcher, DirectedEdgeMatcher, CaptureEdgeMatcher}
import edu.washington.cs.knowitall.collection.immutable.Bag
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.common.enrich.Traversables.traversableOncePairTo
import edu.washington.cs.knowitall.openparse.{SlotMatcher, RelationMatcher, ExtractorPattern, ArgumentMatcher}
import edu.washington.cs.knowitall.tool.parse.graph.{RegexNodeMatcher, RegexEdgeMatcher, PostagNodeMatcher, LabelEdgeMatcher, DependencyPattern, DependencyNode}
import edu.washington.cs.knowitall.tool.postag.Postagger
import edu.washington.cs.knowitall.ollie.Ollie.stemmer

import scalaz.Scalaz._
import scalaz._
import scopt.OptionParser

/** A main method for building template extractors from
  * a bootstrapping set of relations, patterns, and their count.
  * 
  * @author Michael Schmitz
  */
object BuildTemplates {
  val logger = LoggerFactory.getLogger(this.getClass)

  abstract class Settings {
    def sourceFile: File
    def destFile: Option[File]
    def templateFile: Option[File]
    def minCount: Int
    def relationSemantics: Boolean
    def slotSemantics: Boolean

    def debug: Option[File]
    def fromHistogram: Boolean

    def filterNnEdge: Boolean
    def filterAmodEdge: Boolean
    def filterSideRel: Boolean
    def filterPrepMismatch: Boolean

    val minimumSemanticsCount = 5
  }

  def main(args: Array[String]) {
    val settings = new Settings {
      var sourceFile: File = null
      var destFile: Option[File] = None
      var templateFile: Option[File] = None
      var minCount: Int = 5
      var semantics: Boolean = false

      var debug: Option[File] = None
      var fromHistogram: Boolean = false

      var filterNnEdge = false
      var filterAmodEdge = false
      var filterSideRel = false
      var filterPrepMismatch = false

      var relationSemantics = false
      var slotSemantics = false
    }

    val parser = new OptionParser("buildtemp") {
      arg("source", "file with source relation, pattern pairs", { path: String => settings.sourceFile = new File(path) })
      argOpt("dest", "optional parameter to specify output to a file", { path: String => settings.destFile = Some(new File(path)) })
      opt("t", "reltemplates", "relation templates", { path: String => settings.templateFile = Some(new File(path)) })
      intOpt("n", "minimum", "minimum frequency for a pattern", { min: Int => settings.minCount = min })

      opt("relation-semantics", "add lexical restrictions", { settings.relationSemantics = true })
      opt("slot-semantics", "add lexical restrictions", { settings.slotSemantics = true })
      opt("filter-nnedge", "filter nn edges", {settings.filterNnEdge = true} )
      opt("filter-amodedge", "filter amod edges", {settings.filterAmodEdge = true} )
      opt("filter-siderel", "filter relation on side", {settings.filterSideRel = true} )
      opt("filter-prepmismatch", "filter prep mismatch", {settings.filterPrepMismatch = true} )

      opt("from-histogram", "input is the histogram", { settings.fromHistogram = true })
      opt("d", "debug", "directory to output debug files", { path: String => settings.debug = Some(new File(path)) })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }

  def order(items: TraversableOnce[((Any, Any), Attrib)]) =
    items.toSeq.sortBy(item => (-item._2.count, item._1.toString))

  def output(file: File, items: TraversableOnce[((Any, Any), Attrib)]): Unit = {
    using (new PrintWriter(file, "UTF8")) { pw =>
      outputDetailed(pw, items)
    }
  }

  def output(writer: PrintWriter, items: TraversableOnce[((Any, Any), Attrib)]): Unit = {
    items.foreach {
      case ((rel, pattern), attrib) =>
        writer.println(rel + "\t" + pattern + "\t" + attrib.count)
    }
  }

  def outputDetailed(file: File, items: TraversableOnce[((Any, Any), Attrib)]): Unit = {
    using (new PrintWriter(file, "UTF8")) { pw =>
      outputDetailed(pw, items)
    }
  }

  def outputDetailed(writer: PrintWriter, items: TraversableOnce[((Any, Any), Attrib)]): Unit = {
    items.foreach {
      case ((rel, pattern), attrib) =>
        writer.println(rel + "\t" + pattern + "\t" + attrib.count + "\t" + attrib.slots.asMap.mkString("\t"))
    }
  }

  def outputLookup[K, V](file: File, items: Iterable[(K, Iterable[V])]) = {
    using (new PrintWriter(file, "UTF8")) { pw =>
      items.foreach { case (key, values) =>
        pw.println(key+"\t"+values.mkString("\t"))
      }
    }
  }

  case class Attrib(count: Int, slots: Bag[String] = Bag.empty, rels: immutable.SortedSet[String] = immutable.SortedSet.empty, mismatch: Boolean = false) {
    def plus(that: Attrib) = Attrib(this.count + that.count, this.slots merge that.slots, this.rels ++ that.rels, this.mismatch | that.mismatch)
  }
  implicit def AttribSemigroup[T]: Semigroup[Attrib] = semigroup(_ plus _)
  implicit def AttribZero[T]: Zero[Attrib] = zero(Attrib(0, Bag.empty[String], immutable.SortedSet.empty[String]))


  def run(settings: Settings) {
    val prepRegex = new Regex("^(.*?)\\s+((?:"+Postagger.prepositions.map(_.replaceAll(" ", "_")).mkString("|")+"))$")

    def relPrep(rel: String) = {
      rel match {
        case prepRegex(rel, prep) => Some(prep)
        case rel => None
      }
    }

    def relationOnSide: PartialFunction[ExtractorPattern, Boolean] =
    { case pattern =>
        pattern.nodeMatchers.head.isInstanceOf[RelationMatcher] ||
        pattern.nodeMatchers.tail.isInstanceOf[RelationMatcher]
    }

    def nnEdge: PartialFunction[ExtractorPattern, Boolean] =
    { case pattern => pattern.baseEdgeMatchers.exists {
        case m: LabelEdgeMatcher => m.label == "nn"
        case _ => false
      }
    }

    def amodEdge: PartialFunction[ExtractorPattern, Boolean] =
    { case pattern => pattern.baseEdgeMatchers.exists {
        case m: LabelEdgeMatcher => m.label == "amod"
        case _ => false
      }
    }

    def prepMismatch: PartialFunction[((String, ExtractorPattern)), Boolean] =
    { case ((rel, pattern)) =>
        relPrep(rel) match {
          case Some(relationPrep) =>
            val edgePreps = pattern.baseEdgeMatchers.collect {
              case m: LabelEdgeMatcher if m.label startsWith "prep_" => m.label.drop(5).replaceAll("_", " ")
            }
            edgePreps.exists(_ != relationPrep)
          case None => false
        }
    }

    // extract lemmas from a relation string
    def baseRelLemmas(rel: String): Option[String] = {
      def clean(rel: String) = {
        rel.split("\\s+").iterator.filterNot(_.isEmpty).filterNot(_=="be").filterNot(_=="{prep}").toSeq.lastOption.map(Some(_)).getOrElse(None)
      }
      rel match {
        case prepRegex(rel, prep) => clean(rel)
        case rel => clean(rel)
      }
    }

    // lookup a relation in a map to get the template
    val templatesMap = settings.templateFile.map { templateFile =>
      using(Source.fromFile(templateFile)) { source =>
        source.getLines.map { line =>
          val Array(rel, template) = line.split("\t")
          (rel, template)
        }.toMap.withDefault((x: String)=>x)
      }
    }.getOrElse(Map().withDefault((x: String)=>x))

    // build a template from the supplied relation
    def buildTemplate(string: String) = {
      // build a template from the supplied relation WITHOUT a preposition
      def templateFromRel(rel: String) =
        rel.split("\\s+").iterator.map {
          case s@"be" => s
          case s@"{prep}" => s
          case s => "{rel}"
        }.mkString(" ")

      templatesMap.get(string).getOrElse {
        string match {
          case prepRegex(rel, prep) =>
            templateFromRel(rel) + " " + prep
          case rel => templateFromRel(rel)
        }
      }
    }

    def buildHistogram(file: File) = {
      val serializedHistogram =
        using(Source.fromFile(file)) { source =>
          if (settings.fromHistogram) {
            // the input is the histogram so just read it
            // this fork exists because creating this histogram
            // is the slowest part of building templates.
            source.getLines.map { line =>
              val Array(rel, pattern, count, slots @ _*) = line.split("\t")
              ((rel, pattern), Attrib(count.toInt, slots=Bag.from(slots)))
            }.toMap
          } else {
            // create the histogram from the patterned file
            source.getLines.map { line =>
              val Array(rel, _, _, _, pattern, _, _, slots @ _*) = line.split("\t")
              val slotBag: Bag[String] =
                if (settings.slotSemantics) Bag.from(slots.iterator.filter(_.forall(_.isLetter)))
                else Bag.empty
              ((rel, pattern), Attrib(1, slotBag, immutable.SortedSet.empty))
            }.mergeKeys
          }
        }

      // deserialize the patterns after creating the histogram
      // for efficiency
      val histogram = serializedHistogram.map {
        case ((rel, pattern), attrib) =>
          ((rel, new ExtractorPattern(DependencyPattern.deserialize(pattern))), attrib)
      }
      assume(serializedHistogram.size == histogram.size)

      histogram
    }

    def generalizePrepositions(histogram: Iterable[((String, ExtractorPattern), Attrib)]) = {
      val result = histogram.iterator.map {
        case item @ ((rel, pattern), attrib) =>
          val relPrepOption = relPrep(rel)

          val patternContainsPrep = pattern.baseEdgeMatchers.exists {
            case m: LabelEdgeMatcher if m.label startsWith "prep_" => true
            case _ => false
          }

          val mismatch = prepMismatch(rel, pattern) || patternContainsPrep && !relPrepOption.isDefined

          val template = rel match {
            case prepRegex(rel, prep) =>
              // if the pattern contains a preposition too, substitute
              // with the capture group name
              if (patternContainsPrep && !mismatch) rel + " {prep}"
              // otherwise, keep the preposition
              else rel + " " + prep
            case _ => rel
          }

          def target(relPrep: String)(m: Matcher[DependencyNode]): Boolean = m match {
            case m: DirectedEdgeMatcher[_] =>
              m.matcher match {
                case sub: LabelEdgeMatcher if sub.label.startsWith("prep_") => sub.label.drop(5).replaceAll("_", " ") == relPrep
                case _ => false
              }
            case _ => false
          }

          import scalaz._
          import Scalaz._

          val newMatchers =
            if (!patternContainsPrep || !relPrepOption.isDefined || mismatch) None
            else
              (for {
                relPrep <- relPrepOption
                zipper <- pattern.matchers.zipperEnd
                found <- zipper findPrevious target(relPrep)
              } yield {
                found.modify { m =>
                  new CaptureEdgeMatcher[DependencyNode]("prep",
                    new DirectedEdgeMatcher[DependencyNode](m.asInstanceOf[DirectedEdgeMatcher[_]].direction,
                      new RegexEdgeMatcher("prep_(.*)".r)))
                }.toStream.toList
              }).orElse(throw new IllegalStateException(rel + " -> " + relPrepOption.toString + " -> " + pattern))

          ((template, newMatchers.map(matchers => new ExtractorPattern(new DependencyPattern(matchers))).getOrElse(pattern)), attrib.copy(mismatch=mismatch))
      }.mergeKeys

      assume(result.values.iterator.map(_.count).sum == histogram.iterator.map(_._2.count).sum)
      result
    }

    def generalizeRelation(histogram: Iterable[((String, ExtractorPattern), Attrib)]) = {
      // we need to handle semantics
      val groups = histogram.toSeq.map {
        case item @ ((rel, pattern), attrib) =>
          val template = buildTemplate(rel)
          ((template, pattern), if (settings.relationSemantics) attrib.copy(rels = immutable.SortedSet[String]() ++ baseRelLemmas(rel)) else attrib)
      }.toSeq.groupBy(_._1).flatMap {
        case (key @ (template, pattern), seqs) =>
          val attribs = seqs.map(_._2)
          val attrib = attribs.reduce(_ plus _)
          if (settings.relationSemantics && (nnEdge(pattern) || amodEdge(pattern) || relationOnSide(pattern) || attrib.mismatch)) {
            val values = attribs.filter(_.count > settings.minimumSemanticsCount)
            if (values.isEmpty || values.iterator.flatMap(_.rels).isEmpty) None
            else Some((key, (true, values.reduce(_ plus _))))
          } else {
            Some(key, (false, attrib))
          }
      }

      val result = groups.map {
        case ((template, pattern), (true, attrib)) =>
          val nnedge = nnEdge(pattern)
          val regex = attrib.rels.toSeq.mkString("|").r
          val matchers = pattern.matchers.map {
            case m: RelationMatcher => new RelationMatcher("rel", new ConjunctiveNodeMatcher(Set(m.matcher, new RegexNodeMatcher(regex))))
            case m: ArgumentMatcher if nnedge => new ArgumentMatcher(m.alias, new PostagNodeMatcher("NNP"))
            case m => m
          }
          ((template, new ExtractorPattern(matchers)), attrib)
        case ((template, pattern), (false, attrib)) =>
          ((template, pattern), attrib)
      }.mergeKeys

      if (!settings.relationSemantics) {
        assume(result.values.iterator.map(_.count).sum == histogram.iterator.map(_._2.count).sum)
      }

      result
    }

    def addSlotSemantics(histogram: Iterable[((String, ExtractorPattern), Attrib)]) = {
      if (settings.slotSemantics) {
        histogram.flatMap { case ((rel, pattern), attrib) =>
          val hasSlot = pattern.matchers.exists(_.isInstanceOf[SlotMatcher])

          if (!hasSlot) Some(((rel, pattern), attrib))
          else {
            val semantics = attrib.slots.asMap.filter(_._2 >= 5)

            if (semantics.isEmpty) None
            else {
	          val matchers = pattern.matchers.map {
	            case m: SlotMatcher =>
	              new SlotMatcher(m.alias, new ConjunctiveNodeMatcher(m.matcher, new RegexNodeMatcher(semantics.keys.toSeq.sorted.mkString("|").r)))
	            case m => m
	          }

	          Some((rel, new ExtractorPattern(matchers)), attrib)
            }
          }
        }
      }
      else histogram
    }

    logger.info("Building histogram...")
    val histogram = buildHistogram(settings.sourceFile)
    settings.debug map { dir =>
      outputDetailed(new File(dir, "histogram.txt"), order(histogram))
    }

    logger.info("Removing bad templates...")
    val filtered = histogram filterNot { case ((rel, pattern), count) =>
      settings.filterNnEdge && nnEdge(pattern) ||
      settings.filterAmodEdge && amodEdge(pattern) ||
      settings.filterSideRel && relationOnSide(pattern) ||
      settings.filterPrepMismatch && prepMismatch((rel, pattern))
    }
    settings.debug.map { dir =>
    output (new File(dir, "filtered-keep.txt"), filtered)
    //output (new File(dir, "filtered-del-edge.txt"), (histogram.iterator filter relationOnSide).toSeq.sortBy(-_._2))
    //output (new File(dir, "filtered-del-nn.txt"), (histogram.iterator filter nnEdge).toSeq.sortBy(-_._2))
    //output (new File(dir, "filtered-del-prepsmatch.txt"), (histogram.iterator filterNot prepsMatch).toSeq.sortBy(-_._2))
    }
    logger.info((histogram.values.iterator.map(_.count).sum - filtered.values.iterator.map(_.count).sum).toString + " items removed.")

    logger.info("Generalizing prepositions...")
    val generalizedPreposition = generalizePrepositions(filtered)
    settings.debug.map { dir =>
      output(new File(dir, "generalized-prep.txt"), order(generalizedPreposition))
    }

    logger.info("Generalizing relations...")
    val generalizedRelation = generalizeRelation(generalizedPreposition)
    settings.debug.map { dir =>
      output(new File(dir, "generalized-rel.txt"), order(generalizedRelation))
    }

    logger.info("Adding slot semantics...")
    val withSlotSemantics = addSlotSemantics(generalizedRelation)
    settings.debug.map { dir =>
      output(new File(dir, "semantics-slot.txt"), order(generalizedRelation))
    }

    val cleaned = withSlotSemantics
      .filter {
        case ((template, pat), attrib) =>
          attrib.count >= settings.minCount
      }
      .filter {
        case ((template, pattern), attrib) =>
          // remove {rel} {rel} templates for now
          template.split("\\s+").count(_ == "{rel}") <= 1
      }
    settings.debug.map { dir =>
      output(new File(dir, "cleaned.txt"), order(cleaned))
    }

    logger.info("Removing duplicates...")
    val dedup = cleaned.groupBy { case ((template, pat), attrib) =>
      def filter(matchers: Iterable[Matcher[_]]) = matchers.filter {
        case m: ArgumentMatcher => false
        case _ => true
      }
      // set the key to the matchers, without arg1 and arg2,
      // and the reflection's matchers so we don't have mirrored
      // patterns
      Set(filter(pat.matchers), filter(pat.reflection.matchers))
    }.mapValues(_.maxBy(_._2.count)).values
    settings.debug.map { dir =>
      output(new File(dir, "dedup.txt"), order(dedup))
    }

    logger.info("Writing templates...")
    using {
      settings.destFile match {
        case Some(file) => new PrintWriter(file)
        case None => new PrintWriter(System.out)
      }
    } { writer =>
      output(writer, order(dedup))
    }
  }
}
