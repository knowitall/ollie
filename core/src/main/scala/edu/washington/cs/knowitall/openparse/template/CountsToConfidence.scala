package edu.washington.cs.knowitall.openparse.template

import scopt.mutable.OptionParser
import java.io.File
import edu.washington.cs.knowitall.common.Resource.using
import scala.io.Source
import java.io.PrintWriter

object CountsToConfidence {
  abstract class Settings {
    def sourceFile: File
    def destFile: Option[File]
  }
  
  def main(args: Array[String]) = {
    object settings extends Settings {
      var sourceFile: File = _
      var destFile: Option[File] = None
    }
    
    val parser = new OptionParser("convertconf") {
      arg("source", "file with pattern, count pairs", { path: String => settings.sourceFile = new File(path) })
      argOpt("dest", "optional parameter to specify output to a file", { path: String => settings.destFile = Some(new File(path)) })
    }
    
    if (parser.parse(args)) {
      run(settings)
    }
  }
  
  def run(settings: Settings) = {
    using (Source.fromFile(settings.sourceFile)) { source =>
      using (
        settings.destFile match {
          case Some(file) => new PrintWriter(file)
          case None => new PrintWriter(System.out)
        }
      ) { output =>
        val lines = {
          val it = source.getLines
          val first = it.next
          output.println(first)
          it.toList
        }
        
        val max = lines.map(_.split("\t").last.toInt).max

        for (line <- lines) {
          val parts = line.split("\t")
          val count = parts.last.toInt
          output.println(parts.take(parts.length - 1).mkString("\t") + "\t" + ("%1.4f" format (count.toDouble / max.toDouble)))
        }
      }
    }
  }
}