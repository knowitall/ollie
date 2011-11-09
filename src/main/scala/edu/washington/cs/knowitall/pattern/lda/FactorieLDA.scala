/*
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}
import scala.util.matching.Regex
import scala.io.Source
import java.io.File
import cc.factorie._
import cc.factorie.generative._
import cc.factorie.app.strings.Stopwords
import cc.factorie.app.strings.alphaSegmenter

object FactorieLDA {
  val numTopics = 50
  object ZDomain extends DiscreteDomain { def size = numTopics }
  class Z(value: Int = 0) extends cc.factorie.generative.Gate(value) { def domain = ZDomain }
  object WordDomain extends CategoricalDomain[String]
  class Word(value: String) extends Categorical(value) { def domain = WordDomain; def z = parentFactor.asInstanceOf[DiscreteMixture.Factor]._3 }
  class Document(val value: String) extends ArrayBuffer[Word] {var theta: DenseCountsProportions = null}
  val beta = new GrowableUniformMasses(WordDomain, 0.1)

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile(args(0))
    val phis = Mixture(numTopics)(new GrowableDenseCountsProportions(WordDomain) ~ Dirichlet(beta))
    val alphas = new DenseMasses(numTopics, 0.1)

    println("reading input...")
    var relations: Map[String, List[String]] = Map()
    for (line <- source.getLines) {
      val Array(rel, arg1, arg2, pattern, slot1, slot2) = line.split("\t", -1)
      val patterns = relations.getOrElse(rel, List.empty)
      relations += rel -> (pattern :: patterns)
    }

    println("building model...")
    val documents = new ArrayBuffer[Document]
    for ((rel, patterns) <- relations) {
      val doc = new Document(rel)

      doc.theta = new DenseCountsProportions(numTopics) ~ Dirichlet(alphas)
      for (word <- patterns) {
        val z = new Z :~ Discrete(doc.theta)
        doc += new Word(word) ~ DiscreteMixture(phis, z)
      }

      documents += doc
    }

    println("running interations...")
    val collapse = new ArrayBuffer[GeneratedVar]
    collapse += phis
    collapse ++= documents.map(_.theta)
    val sampler = new CollapsedGibbsSampler(collapse)
    val startTime = System.currentTimeMillis
    for (i <- 1 to 20) {
      println("Iteration " + i)
      for (doc <- documents; word <- doc) sampler.process(word.z)
      if (i % 5 == 0) {
        sampler.export()
        // Turned off hyperparameter optimization
        //DirichletMomentMatching.estimate(alphaMean, alphaPrecision)
        //println("alpha = " + alphaMean.map(_ * alphaPrecision.doubleValue).mkString(" "))
        phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.top(10).map(dp => WordDomain.getCategory(dp.index)).mkString(" ")))
        println
      }
    }
    //phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")

  }
}
*/
