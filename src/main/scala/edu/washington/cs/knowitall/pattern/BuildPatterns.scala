package edu.washington.cs.knowitall
package pattern

import java.io.File
import java.io.PrintWriter
import scala.io.Source
import scala.collection
import common.Timing._
import tool.parse._
import tool.stem._
import tool.parse.pattern._
import tool.parse.graph._
import org.slf4j.LoggerFactory

import scopt.OptionParser

object BuildPatterns {
  val logger = LoggerFactory.getLogger(this.getClass)

  val CHUNK_SIZE = 100000

  class NoRelationNodeException(message: String) extends NoSuchElementException(message)
  class Settings {
    var sourcePath: String = _
    var destPath: Option[String] = None
    var length = Option.empty[Int]
    var parallel = false
  }

  def main(args: Array[String]) {
    val settings = new Settings

    val parser = new OptionParser("buildpats") {
      arg("source", "source", { v: String => settings.sourcePath = v })
      argOpt("dest", "dest", { v: String => settings.destPath = Some(v) })
      opt("p", "parallel", "run multithreaded", { settings.parallel = true })
      intOpt("l", "length", "<length>", "maximum number of edges in the patterns", { l: Int => settings.length = Some(l) })
    }
    if (parser.parse(args)) {
      logger.debug("info: " + args.mkString(" "))
      main(settings)
    }
  }
 
 def main(settings: Settings) {
   def validGraph(graph: DependencyGraph) = {
     // make sure there is a verb
     graph.nodes.exists(node => "(?i)^VB".r.findFirstIn(node.postag).isDefined)
   }
   
    // file with dependencies
    val source = Source.fromFile(settings.sourcePath, "UTF-8")
    val writer = settings.destPath.map(dest => new PrintWriter(new File(dest), "UTF8")).getOrElse(new PrintWriter(System.out))
    
    logger.info("chunk size: " + CHUNK_SIZE)
    logger.info("pattern length: " + settings.length)

    var index = 0
    for (lines <- source.getLines.grouped(CHUNK_SIZE)) {
      @volatile var count = 0

      val group = if (settings.parallel) lines.par else lines

      val ms = time(group.foreach { line =>
        val Array(rel, arg1, arg2, lemmaString, text, _/*lemmas*/, _/*postags*/, _/*chunks*/, deps) = line.split("\t")
        val lemmas = lemmaString.split("\\s+").toSet

        // todo: push stemming forward in the process
        try {
          val graph = DependencyGraph.deserialize(deps).map { node =>
            node.lemmatize(MorphaStemmer.instance)
          }.collapseNounGroups().collapseNNPOf.simplifyPostags

          if (!validGraph(graph)) {
            logger.warn("Invalid graph (no verb?): " + graph.text + "\t" + graph.serialize)
          }
          else {
            val patterns = findRelationPatterns(graph, rel, arg1, arg2, lemmas, settings.length)
            for ((pattern, slots) <- patterns; if pattern.valid) {
              if (!settings.length.isDefined || pattern.nodeMatchers.length <= settings.length.get) {
                writer.println((List(rel, arg1, arg2, lemmas.mkString(" "), pattern, text, deps) ::: slots).mkString("\t"))
                count += 1
              }
            }
          }
        }
        catch {
          case e: NoRelationNodeException => logger.warn(e.toString)
          case e: DependencyGraph.SerializationException => 
            logger.error("could not deserialize graph: " + deps, e)
        }
      })

      logger.info("chunk " + index + ": " + count + " items in " + Seconds.format(ms))
      writer.flush()

      index += 1
    }

    logger.info("done.")

    source.close
    writer.close
  }
 
   def findBipaths(lemmas: Set[String], graph: DependencyGraph, maxLength: Option[Int]) = {
    // build a set of all the possible combinations that don't use a
    // node with the same text twice
    def combinations(nodes: Set[DependencyNode]) = {
      def rec(nodes: Seq[(String, Set[DependencyNode])], combs: Set[DependencyNode]): Set[Set[DependencyNode]] = nodes match {
        case Seq((text, set), rest @ _*) => set.flatMap(item => rec(rest, combs + item))
        case Seq() => Set(combs)
      }

      if (nodes.map(_.text).size == nodes.size) {
        // there are no duplicate nodes
        Set(nodes)
      } else {
        // build combinations
        rec(nodes.groupBy(_.text).toSeq, Set())
      }
    }

    val allNodes = lemmas.flatMap { lemma =>
      // find all exact matches
      val exacts = graph.graph.vertices.filter(_.text == lemma)

      // or one partial match
      if (exacts.isEmpty) graph.graph.vertices.find(_.text.contains(lemma)) map { List(_) } getOrElse List.empty
      else exacts
    }

    combinations(allNodes).flatMap { nodes =>
      val paths = graph.graph.bipaths(nodes, maxLength)

      // restrict to paths that go up and then down
      paths.filter(bipath => bipath.path.length > 0 && 
          bipath.path.dropWhile(_.isInstanceOf[UpEdge[_]]).dropWhile(_.isInstanceOf[DownEdge[_]]).isEmpty)
    }
  }

  class InvalidBipathException(message: String) extends RuntimeException(message)
  def findPattern(graph: DependencyGraph,
    lemmas: Set[String],
    replacements: Map[String, String],
    maxLength: Option[Int]) = {

    def valid(bip: Bipath[DependencyNode]) = {
      def params = replacements.toString+"; "+bip.toString

      // we don't have any "punct" edges
      !bip.edges.find(_.label == "punct").map { edge =>
        logger.debug("invalid: punct edge '"+edge+"': "+bip)
      }.isDefined &&
      // we don't have any "dep" edges
      !bip.edges.find(_.label == "dep").map { edge =>
        logger.debug("invalid: dep edge '"+edge+"': "+bip)
      }.isDefined &&
      // all edges are simple word characters
      !bip.edges.find(!_.label.matches("\\w+")).map { edge =>
        logger.debug("invalid: special character in edge '"+edge+"': "+bip)
      }.isDefined
    }

    // find paths containing lemma
    val bipaths = findBipaths(lemmas, graph, maxLength) filter valid

    bipaths.flatMap { bip =>
      import scalaz._
      import Scalaz._

      val zipper = DependencyPattern.create(bip).matchers.toZipper.get

      val zipperReplaced = try { Some((zipper /: replacements){ 
        case (zipper, (target, rep)) =>
          val zipperMatch = 
            // find an exact match
            zipper.findZ {
              case m: DependencyNodeMatcher => m.text == target
              case _ => false
           } orElse
            // find a partial match
            zipper.findZ {
              case m: DependencyNodeMatcher => m.text contains target
              case _ => false
            } getOrElse {
              throw new InvalidBipathException("invalid: couldn't find replacement '"+rep+"': "+bip)
            }

          // ensure valid postags
          if (!OpenParse.VALID_ARG_POSTAG.contains(zipperMatch.focus.asInstanceOf[DependencyNodeMatcher].postag))
            throw new InvalidBipathException("invalid: invalid arg postag '"+zipper.focus+"': "+bip)

          // make replacements
          zipperMatch.update(new ArgumentMatcher(rep))
      })} catch {
        case e: InvalidBipathException => logger.debug(e.getMessage); None
      }

      zipperReplaced.map(zipper => new ExtractorPattern(zipper.toStream.toList))
    }
  }
  
  /**
    * Find patterns in the graph that connect arg1, rel, and arg2.
    * 
    * @param  graph  the graph to find the pattern in.
    * @param  rel  the relation
    * @param  arg1  the argument 1
    * @param  arg2  the argument 2
    * @param  lemmas  all lemmas in `rel`, `arg1`, and `arg2`
    * @param  maxLength  the maximum path length, in edges
    */
  def findRelationPatterns(graph: DependencyGraph, rel: String, arg1: String, arg2: String, lemmas: Set[String], maxLength: Option[Int] = None) = {
    findPatterns(graph, lemmas, Map(arg1 -> "arg1", arg2 -> "arg2"), rel, maxLength)
  }

  def findPatterns(graph: DependencyGraph, lemmas: Set[String], replacements: Map[String, String], rel: String, maxLength: Option[Int]): List[(ExtractorPattern, List[String])] = {
    def valid(pattern: Pattern[DependencyNode]) = 
      // make sure arg1 comes first
      pattern.matchers.find(_
        .isInstanceOf[CaptureNodeMatcher[_]]).map(_
        .asInstanceOf[CaptureNodeMatcher[_]].alias == "arg1").getOrElse(false)
      
    val patterns = findPattern(graph, lemmas, replacements, maxLength)
    
    val filtered = patterns.filter(valid).toList

    val relLemmas = rel.split(" ").toList filter lemmas

    // find the best part to replace with rel
    filtered.map { pattern =>
      import scalaz._
      import Scalaz._

      def replaceRels(zipper: Zipper[Matcher[DependencyNode]]) = {
        def replaceRels(zipper: Zipper[Matcher[DependencyNode]], rels: List[(String, Int)]): Zipper[Matcher[DependencyNode]] = rels match {
          case Nil => zipper
          case (rel, i) :: xs => 
            // find the rel node
            val relZipper = zipper.findZ(_ match {
              case nm: DependencyNodeMatcher => nm.text.split("\\s+").contains(rel)
              case _ => false
            }) getOrElse {
              throw new NoRelationNodeException("No relation node ("+rel+") in pattern: " + pattern)
            }
    
            // replace rel
            val postag = relZipper.focus.asInstanceOf[DependencyNodeMatcher].postag
            val alias = if (i == 0 && xs.isEmpty) "rel" else "rel" + i
            val updated = relZipper.update(new CaptureNodeMatcher(alias, new PostagNodeMatcher(postag)))
            
            replaceRels(updated, xs)
        }
        
        replaceRels(zipper, relLemmas.zipWithIndex)
      }

      def replaceSlots(zipper: Zipper[Matcher[DependencyNode]]) = {
        def replaceSlots(zipper: Zipper[Matcher[DependencyNode]], labels: List[String], index: Int): (Zipper[Matcher[DependencyNode]], List[String]) = {
          def replaceSlot(zipper: Zipper[Matcher[DependencyNode]]) = {
            val node = zipper.focus.asInstanceOf[DependencyNodeMatcher]
            val postag = node.postag
            (Scalaz.zipper[Matcher[DependencyNode]](zipper.lefts, new CaptureNodeMatcher("slot"+index, new PostagNodeMatcher(postag)), zipper.rights),
                node.text)
          }

          zipper.findZ(_.isInstanceOf[DependencyNodeMatcher]) match {
            case Some(z) => val (zipper, label) = replaceSlot(z)
                    replaceSlots(zipper, label :: labels, index + 1)
            case None => (zipper, labels)
          }
        }

        replaceSlots(zipper, List(), 0)
      }

      val zipper = pattern.matchers.toZipper.get
      val relZipper = replaceRels(zipper)
      val (slotZipper, slotLabels) = replaceSlots(relZipper)

      (new ExtractorPattern(slotZipper.toStream.toList), slotLabels)
    }
  }
}

object KeepCommonPatterns {
  def main(args: Array[String]) {
    val min = args(1).toInt
    System.err.println("minimum pattern ocurrence: "+min)

    var rows = 0
    var keepers = 0

    var patterns = collection.immutable.Map[String, Int]().withDefaultValue(0)
    val firstSource = Source.fromFile(args(0), "UTF8")
    for (line <- firstSource.getLines) {
      val Array(rel, arg1, arg2, lemmas, pattern, text, deps, _*) = line.split("\t")
      rows += 1
      patterns += pattern -> (patterns(pattern) + 1)
    }
    firstSource.close()

    System.err.println(rows+" rows")
    System.err.println(patterns.size+" unique patterns")

    val secondSource = Source.fromFile(args(0), "UTF8")
    for (line <- secondSource.getLines) {
      val Array(rel, arg1, arg2, lemmas, pattern, text, deps, _*) = line.split("\t")
      if (patterns(pattern) >= min) {
        keepers += 1
        println(line)
      }
    }
    secondSource.close()

    System.err.println(keepers+" patterns that occur more than "+min+"times") 
  }
}

object KeepDiversePatterns {
  abstract class Settings {
    def inputFile: File
    def min: Int
    def outputFile: Option[File]
    def debugFile: Option[File]
  }
  
  def main(args: Array[String]) {
    val settings = new Settings {
      var inputFile: File = _
      var min: Int = 5
      var outputFile: Option[File] = None
      var debugFile: Option[File] = None
    }

    val parser = new OptionParser("buildpats") {
      arg("input", "input file", { path: String => settings.inputFile = new File(path) })
      intOpt("min", "minimum number of relations per pattern", { string: Int => settings.min })
      opt("debug", "debug output file", { path: String => settings.debugFile = Some(new File(path)) })
      opt("output", "output file", { path: String => settings.outputFile = Some(new File(path)) })
    }
    if (parser.parse(args)) {
      run(settings)
    }
  }
    
  def run(settings: Settings) {
    val min = settings.min
    System.err.println("minimum relations per pattern: "+min)

    var rows = 0
    var keepers = 0

    var patterns = collection.immutable.Map[String, Set[Int]]().withDefaultValue(Set())
    val firstSource = Source.fromFile(settings.inputFile, "UTF8")
    for (line <- firstSource.getLines) {
      val Array(rel, arg1, arg2, lemmas, pattern, text, deps, _*) = line.split("\t")
      rows += 1
      patterns += pattern -> (patterns(pattern) + rel.hashCode)
    }
    firstSource.close()

    System.err.println(rows+" rows")
    System.err.println(patterns.size+" unique patterns")
    
    val secondSource = Source.fromFile(settings.inputFile, "UTF8")
    val outputWriter = settings.outputFile.map(new PrintWriter(_)).getOrElse(new PrintWriter(System.out))
    val debugWriter = settings.debugFile.map(new PrintWriter(_))
    for (line <- secondSource.getLines) {
      val Array(rel, arg1, arg2, lemmas, pattern, text, deps, _*) = line.split("\t")
      val size = patterns(pattern).size
      if (size >= min) {
        keepers += 1
        outputWriter.println(line)
      }
      else {
        debugWriter.map(_.println(size+"\t"+pattern))
      }
    }
    debugWriter.map(_.close())
    outputWriter.close()
    secondSource.close()

    System.err.println(keepers+" patterns that occur more than "+min+"times") 
  }
}
