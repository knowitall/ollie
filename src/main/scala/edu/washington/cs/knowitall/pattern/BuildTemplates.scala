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

object BuildTemplates {
  val logger = LoggerFactory.getLogger(this.getClass)
  
  abstract class Settings {
    def debug: Option[File]
    def sourceFile: File
    def destFile: Option[File]
    def templateFile: Option[File]
    def minCount: Int
  }
  def main(args: Array[String]) {
    val settings = new Settings {
      var sourceFile: File = null
      var destFile: Option[File] = None
      var debug: Option[File] = None
      var templateFile: Option[File] = None
      var minCount: Int = 5
    }
    
    val parser = new OptionParser("buildtemp") {
      arg("source", "file with source relation, pattern pairs", { path: String => settings.sourceFile = new File(path) })
      argOpt("dest", "optional parameter to specify output to a file", { path: String => settings.destFile = Some(new File(path)) })
      opt("t", "reltemplates", "relation templates", { path: String => settings.templateFile = Some(new File(path)) })
      opt("d", "debug", "directory to output debug files", { path: String => settings.debug = Some(new File(path)) })
      intOpt("n", "minimum", "minimum frequency for a pattern", { min: Int => settings.minCount = min })
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
  
  def run(settings: Settings) {
    val templatesMap = settings.templateFile.map { templateFile =>
      using(Source.fromFile(templateFile)) { source =>
        source.getLines.map { line =>
          val Array(rel, template) = line.split("\t")
          (rel, template)
        }.toMap.withDefault((x: String)=>x)
      }
    }.getOrElse(Map().withDefault((x: String)=>x))
    
    val prepRegex = new Regex("^(.*?)\\s+((?:"+PosTagger.prepositions.map(_.replaceAll(" ", "_")).mkString("|")+"))$")
    def templateFromRel(rel: String) =
      rel.split("\\s+").iterator.map {
        case "be" => "be"
        case s => "{rel}"
      }.mkString(" ")
    def buildTemplate(string: String) =
      templatesMap.get(string).getOrElse {
        string match {
          case prepRegex(rel, prep) =>
            templateFromRel(rel) + " " + prep
          case rel => templateFromRel(rel)
        }
      }
    
    logger.info("Building histogram...")
    val histogram = 
      using (Source.fromFile(settings.sourceFile)) { source =>
        source.getLines.map { line =>
          val Array(rel, _, _, _, pattern, _*) = line.split("\t")
          (buildTemplate(rel), pattern)
        }.histogram
      }.map { case ((rel, pattern), count) => 
        ((rel, new ExtractorPattern(DependencyPattern.deserialize(pattern))), count) 
      }

    settings.debug.map { dir =>
      output(new File(dir, "histogram.txt"), histogram.toSeq.sortBy(-_._2))
    }

    logger.info("Removing bad templates...")
    val relationOnEdge: PartialFunction[((String, ExtractorPattern), Int), Boolean] =  
    { case ((rel, pattern), count) =>
        pattern.nodeMatchers.head.isInstanceOf[RelationMatcher] ||
        pattern.nodeMatchers.tail.isInstanceOf[RelationMatcher]
    }  
        
    val nnEdge: PartialFunction[((String, ExtractorPattern), Int), Boolean] = 
    { case ((rel, pattern), count) => pattern.depEdgeMatchers.exists { 
        case m: LabelEdgeMatcher => m.label == "nn"
        case _ => false
      }
    }
      
    val prepsMatch: PartialFunction[((String, ExtractorPattern), Int), Boolean] = 
    { case ((rel, pattern), count) => 
        val relPrep = rel.split(" ").last
        val edgePreps = pattern.depEdgeMatchers.collect {
          case m: LabelEdgeMatcher if m.label startsWith "prep_" => m.label.drop(5)
        }
        edgePreps.forall(_ == relPrep)
    }
      //!relationOnEdge && !nnEdge && prepsMatch
    val filtered = histogram filterNot relationOnEdge filterNot nnEdge filter prepsMatch
      
    settings.debug.map { dir =>
	  output (new File(dir, "filtered-keep.txt"), filtered.toSeq.sortBy(-_._2))
	  output (new File(dir, "filtered-del-edge.txt"), (histogram.iterator filter relationOnEdge).toSeq.sortBy(-_._2))
	  output (new File(dir, "filtered-del-nn.txt"), (histogram.iterator filter nnEdge).toSeq.sortBy(-_._2))
	  output (new File(dir, "filtered-del-prepsmatch.txt"), (histogram.iterator filterNot prepsMatch).toSeq.sortBy(-_._2))
    }
    
    logger.info("Generalizing templates...")
    val generalized = filtered.iterator.map { case((rel, pattern), count) =>
      val containsPrep = pattern.depEdgeMatchers.exists {
        case m: LabelEdgeMatcher if m.label startsWith "prep_" => true
        case _ => false
      }
      
      val template = rel match {
        case prepRegex(rel, prep) =>
          // if the pattern contains a preposition too, substitute
          // with the capture group name
          if (containsPrep) buildTemplate(rel)+" {prep}"
          // otherwise, keep the preposition
          else buildTemplate(rel)+" "+prep
        case _ => buildTemplate(rel)
      }
      val matchers = pattern.matchers.map {
        case m: DirectedEdgeMatcher[_] => 
          m.matcher match {
            case sub: LabelEdgeMatcher if sub.label.startsWith("prep_") => new CaptureEdgeMatcher("prep", 
                new DirectedEdgeMatcher(m.direction, 
                    new RegexEdgeMatcher("prep_(.*)".r)))
            case sub => m
          }
        case m => m
      }
      ((template, new DependencyPattern(matchers)), count)
    }.histogramFromPartials.filter { case ((template, pat), count) => 
      count >= settings.minCount
    }.filter { case ((template, pattern), count) =>
      // remove {rel} {rel} templates for now
      template.split("\\s+").count(_ == "{rel}") <= 1
    }
    
    settings.debug.map { dir =>
      output(new File(dir, "generalized.txt"), generalized.toSeq.sortBy(-_._2))
    }
    
    logger.info("Removing duplicates...")
    val dedup = generalized.groupBy { case ((template, pat), count) =>
      // set the key to the matchers, without arg1 and arg2,
      // and the reflection's matchers so we don't have mirrored
      // patterns
      Set(pat.matchers.tail.init, pat.reflection.matchers.tail.init)
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
      for (((rel, pattern), count) <- dedup.toSeq.sortBy(-_._2)) {
        writer.println(rel+"\t"+pattern+"\t"+count)
      }
    }
  }
}
