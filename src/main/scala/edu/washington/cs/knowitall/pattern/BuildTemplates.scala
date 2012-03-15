package edu.washington.cs.knowitall.pattern

import java.io.File
import java.io.PrintWriter
import scala.io.Source
import scala.util.matching.Regex
import org.slf4j.LoggerFactory
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.common.enrich.Traversables.traversableOncePairIntTo
import edu.washington.cs.knowitall.common.enrich.Traversables.traversableOnceTo
import edu.washington.cs.knowitall.tool.parse.pattern.CaptureEdgeMatcher
import edu.washington.cs.knowitall.tool.parse.pattern.DependencyPattern
import edu.washington.cs.knowitall.tool.parse.pattern.DirectedEdgeMatcher
import edu.washington.cs.knowitall.tool.parse.pattern.LabelEdgeMatcher
import edu.washington.cs.knowitall.tool.parse.pattern.RegexEdgeMatcher
import edu.washington.cs.knowitall.tool.postag.PosTagger
import scopt.OptionParser
import edu.washington.cs.knowitall.tool.parse.graph.Direction
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.parse.pattern.RegexNodeMatcher
import scala.collection.immutable
import edu.washington.cs.knowitall.tool.parse.pattern.Matcher
import edu.washington.cs.knowitall.tool.parse.pattern.ConjunctiveNodeMatcher
import edu.washington.cs.knowitall.tool.parse.pattern.EdgeMatcher

object BuildTemplates {
  val logger = LoggerFactory.getLogger(this.getClass)
  
  abstract class Settings {
    def sourceFile: File
    def destFile: Option[File]
    def templateFile: Option[File]
    def minCount: Int
    def semantics: Boolean
    
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
    }
    
    val parser = new OptionParser("buildtemp") {
      arg("source", "file with source relation, pattern pairs", { path: String => settings.sourceFile = new File(path) })
      argOpt("dest", "optional parameter to specify output to a file", { path: String => settings.destFile = Some(new File(path)) })
      opt("t", "reltemplates", "relation templates", { path: String => settings.templateFile = Some(new File(path)) })
      intOpt("n", "minimum", "minimum frequency for a pattern", { min: Int => settings.minCount = min })
      
      opt("s", "semantics", "add lexical restrictions", { settings.semantics = true })
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
  
  def output(file: File, items: TraversableOnce[((Any, Any), Any)]) = {
    using (new PrintWriter(file)) { pw =>
      items.foreach { case ((rel, pattern), count) =>
        pw.println(rel+"\t"+pattern+"\t"+count)
      }
    }
  }
  
  def outputLookup[K, V](file: File, items: Iterable[(K, Iterable[V])]) = {
    using (new PrintWriter(file)) { pw =>
      items.foreach { case (key, values) =>
        pw.println(key+"\t"+values.mkString("\t"))
      }
    }
  }
  
  def run(settings: Settings) {
    val prepRegex = new Regex("^(.*?)\\s+((?:"+PosTagger.prepositions.map(_.replaceAll(" ", "_")).mkString("|")+"))$")
    
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
      
    def prepsMatch: PartialFunction[((String, ExtractorPattern)), Boolean] = 
    { case ((rel, pattern)) => 
        relPrep(rel) match {
          case Some(relationPrep) =>
            val edgePreps = pattern.baseEdgeMatchers.collect {
              case m: LabelEdgeMatcher if m.label startsWith "prep_" => m.label.drop(5).replaceAll("_", " ")
            }
            edgePreps.forall(_ == relationPrep)
          case None => true
        }
    }
    
    // extract lemmas from a relation string
    def baseRelLemmas(rel: String): Option[String] = {
      def clean(rel: String) = {
        rel.split("\\s+").iterator.filterNot(_.isEmpty).filterNot(_=="be").filter(_.forall(_.isLetter)).toSeq.lastOption.map(Some(_)).getOrElse(None)
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
              val Array(rel, pattern, count) = line.split("\t")
              ((rel, pattern), count.toInt)
            }.toMap
          } else {
            // create the histogram from the patterned file
            source.getLines.map { line =>
              val Array(rel, _, _, _, pattern, _*) = line.split("\t")
              (rel, pattern)
            }.histogram
          }
        }
      
      // deserialize the patterns after creating the histogram
      // for efficiency
      val histogram = serializedHistogram.map {
        case ((rel, pattern), count) =>
          ((rel, new ExtractorPattern(DependencyPattern.deserialize(pattern))), count)
      }
      assume(serializedHistogram.size == histogram.size)
      
      histogram
    }

    def generalizePrepositions(histogram: Iterable[((String, ExtractorPattern), Int)]) = {
      val result = histogram.iterator.map {
        case item @ ((rel, pattern), count) =>
          val prepositionsMatch = prepsMatch(rel, pattern)
          
          val containsPrep = prepositionsMatch && pattern.baseEdgeMatchers.exists {
            case m: LabelEdgeMatcher if m.label startsWith "prep_" => true
            case _ => false
          }

          val template = rel match {
            case prepRegex(rel, prep) =>
              // if the pattern contains a preposition too, substitute
              // with the capture group name
              if (containsPrep) rel + " {prep}"
              // otherwise, keep the preposition
              else rel + " " + prep
            case _ => rel
          }

          def target(m: Matcher[DependencyNode]): Boolean = m match {
            case m: DirectedEdgeMatcher[_] =>
              m.matcher match {
                case sub: LabelEdgeMatcher if sub.label.startsWith("prep_") => true
                case _ => false
              }
            case _ => false
          }

          import scalaz._
          import Scalaz._
          
          val matchers = 
            if (!prepositionsMatch) pattern.matchers
            else 
              (for {
                zipper <- pattern.matchers.zipperEnd
                found <- zipper findPrevious target
              } yield {
                found.modify { m =>
                  new CaptureEdgeMatcher[DependencyNode]("prep", 
                    new DirectedEdgeMatcher[DependencyNode](m.asInstanceOf[DirectedEdgeMatcher[_]].direction, 
                      new RegexEdgeMatcher("prep_(.*)".r)))
                }.toStream.toList
              }).getOrElse(pattern.matchers)
          
          ((template, new ExtractorPattern(new DependencyPattern(matchers))), count)
      }.mergeHistograms
      
      assume(result.values.sum == histogram.iterator.map(_._2).sum)
      result
    }

    def generalizeRelation(histogram: Iterable[((String, ExtractorPattern), Int)]) = {
        // we need to handle semantics
        val groups = histogram.toSeq.map {
          case item @ ((rel, pattern), count) =>
            val template = buildTemplate(rel)
            ((template, pattern), (baseRelLemmas(rel), count))
        }.toSeq.groupBy(_._1).map { case (key@(template, pattern), members) =>
          if (settings.semantics && (nnEdge(pattern) || amodEdge(pattern) || relationOnSide(pattern))) {
          val values = members.map(_._2).filter(_._2 > settings.minimumSemanticsCount)
          val lemmas: immutable.SortedSet[String] = immutable.SortedSet[String]() ++ values.map(_._1).flatten
          val count: Int = values.map(_._2).sum
          (key, (Some(lemmas), count))
          }
          else {
            (key, (None, members.map(_._2._2).sum))
          }
        }
        
        val result = groups.map { 
          case ((template, pattern), (Some(lemmas), count)) =>
            val regex = lemmas.mkString("|").r
            val matchers = pattern.matchers.map {
              case m: RelationMatcher => new RelationMatcher("rel", new ConjunctiveNodeMatcher(Set(m.matcher, new RegexNodeMatcher(regex))))
              case m => m
            }
            ((template, new ExtractorPattern(matchers)), count)
          case ((template, pattern), (None, count)) =>
	        ((template, pattern), count)
        }.mergeHistograms
      
      if (!settings.semantics) {
        assume(result.values.sum == histogram.iterator.map(_._2).sum)
      }
      
      result
    }
    
    logger.info("Building histogram...")
    val histogram = buildHistogram(settings.sourceFile)
    settings.debug map { dir =>
      output(new File(dir, "histogram.txt"), histogram.toSeq.sortBy(-_._2))
    }

    logger.info("Removing bad templates...")
    val filtered = histogram filterNot { case ((rel, pattern), count) =>
      settings.filterNnEdge && nnEdge(pattern) ||
      settings.filterAmodEdge && amodEdge(pattern) ||
      settings.filterSideRel && relationOnSide(pattern) ||
      settings.filterPrepMismatch && !prepsMatch((rel, pattern))
    }
    settings.debug.map { dir =>
    output (new File(dir, "filtered-keep.txt"), filtered.toSeq.sortBy(-_._2))
    //output (new File(dir, "filtered-del-edge.txt"), (histogram.iterator filter relationOnSide).toSeq.sortBy(-_._2))
    //output (new File(dir, "filtered-del-nn.txt"), (histogram.iterator filter nnEdge).toSeq.sortBy(-_._2))
    //output (new File(dir, "filtered-del-prepsmatch.txt"), (histogram.iterator filterNot prepsMatch).toSeq.sortBy(-_._2))
    }
    logger.info((histogram.values.sum - filtered.values.sum).toString + " items removed.")
    
    logger.info("Generalizing prepositions...")
    val generalizedPreposition = generalizePrepositions(filtered)
    settings.debug.map { dir =>
      output(new File(dir, "generalized-prep.txt"), generalizedPreposition.toSeq.sortBy(-_._2))
    }
    
    logger.info("Generalizing relations...")
    val generalizedRelation = generalizeRelation(generalizedPreposition)
    settings.debug.map { dir =>
      output(new File(dir, "generalized-rel.txt"), generalizedRelation.toSeq.sortBy(-_._2))
    }
    
    val cleaned = generalizedRelation
      .filter {
        case ((template, pat), count) =>
          count >= settings.minCount
      }
      .filter {
        case ((template, pattern), count) =>
          // remove {rel} {rel} templates for now
          template.split("\\s+").count(_ == "{rel}") <= 1
      }
    settings.debug.map { dir =>
      output(new File(dir, "cleaned.txt"), cleaned.toSeq.sortBy(-_._2))
    }
    
    logger.info("Removing duplicates...")
    val dedup = cleaned.groupBy { case ((template, pat), count) =>
      def filter(matchers: Iterable[Matcher[_]]) = matchers.filter {
        case m: ArgumentMatcher => false
        case _ => true
      }
      // set the key to the matchers, without arg1 and arg2,
      // and the reflection's matchers so we don't have mirrored
      // patterns
      Set(filter(pat.matchers), filter(pat.reflection.matchers))
    }.mapValues(_.maxBy(_._2)).values
    settings.debug.map { dir =>
      output(new File(dir, "dedup.txt"), dedup.toSeq.sortBy(-_._2))
    }
    
    logger.info("Writing templates...")
    using {
      settings.destFile match {
        case Some(file) => new PrintWriter(file)
        case None => new PrintWriter(System.out)
      }
    } { writer =>
      for (((rel, pattern), count) <- dedup.toSeq.sortBy(kv => (-kv._2, kv.toString))) {
        writer.println(rel+"\t"+pattern+"\t"+count)
      }
    }
  }
}
