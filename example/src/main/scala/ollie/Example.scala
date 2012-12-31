package ollie

import edu.washington.cs.knowitall.ollie.Ollie
import edu.washington.cs.knowitall.tool.parse.MaltParser
import scala.io.Source
import edu.washington.cs.knowitall.ollie.confidence.OllieIndependentConfFunction

/** This is an example project that takes lines as input from stdin,
  * parses them, runs the Ollie extractor on them, scores the
  * extractions with a confidence function, and then prints the results.
  *
  * You can run this project with the following command:
  *   mvn clean compile exec:java -Dexec.mainClass=ollie.Example
  *
  * You will need to have engmalt.linear-1.7.mco in the base directory
  * of this example for the program to work.  You can download this
  * file from the MaltParser website:
  *
  *   http://www.maltparser.org/mco/english_parser/engmalt.html
  */
object Example extends App {
  val parser = new MaltParser
  val ollie = new Ollie
  val confidence = OllieIndependentConfFunction.loadDefaultClassifier()
  for (line <- Source.stdin.getLines; if !line.trim.isEmpty) {
    val parsed = parser.dependencyGraph(line)
    val extractionInstances = ollie.extract(parsed)

    println("Extractions:")
    for (inst <- extractionInstances) {
      val conf = confidence(inst)
      println(("%.2f" format conf) + "\t" + inst.extraction)
    }
    println("Waiting for next input...")
  }
}