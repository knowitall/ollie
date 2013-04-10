package edu.knowitall.common.enrich

import edu.knowitall.collection.immutable.Bag

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object TraversableSpecTest extends Specification {
  import Traversables._

  "simple histogram works fine" in {
    val h1 = List(1, 2, 2, 3, 3, 3).histogram
    val h2 = List(3, 2, 1, 3, 2, 3).histogram
    h1 must_== h2
    h1 must haveTheSameElementsAs(List((1, 1), (2, 2), (3, 3)))
  }

  "histogram from partials works fine" in {
    val list = List((1, 1), (2, 2), (2, 2), (3, 3), (3, 3), (3, 3))
    val h1 = list.mergeHistograms
    val h2 = list.reverse.mergeHistograms
    val h3 = list.mergeKeys(_ + _)
    h1 must_== h2
    h1 must_== h3
    h1 must haveTheSameElementsAs(List((1, 1), (2, 4), (3, 9)))
  }

  "list multimaps works fine" in {
    val list = List(1 -> 1, 1 -> 2, 1 -> 1, 2 -> 2)
    val multimap = list.toListMultimap

    multimap must haveTheSameElementsAs(Map(1 -> List(1, 2, 1), 2 -> List(2)))

    val extended = (multimap.toSeq :+ (1 -> List(2, 3, 4, 5)))
    val merged = extended.mergeKeys(_ ++ _)

    merged must haveTheSameElementsAs(Map(1 -> List(1, 2, 1, 2, 3, 4, 5), 2 -> List(2)))
  }

  "set multimaps works fine" in {
    val list = List(1 -> 1, 1 -> 2, 1 -> 1, 2 -> 2)
    val multimap = list.toSetMultimap

    multimap must haveTheSameElementsAs(Map(1 -> Set(1, 2), 2 -> Set(2)))

    val extended = (multimap.toSeq :+ (1 -> Set(2, 3, 4, 5)))
    val merged = extended.mergeKeys(_ ++ _)

    merged must haveTheSameElementsAs(Map(1 -> Set(1, 2, 3, 4, 5), 2 -> Set(2)))
  }

  "bag multimaps works fine" in {
    val list = List(1 -> 1, 1 -> 2, 1 -> 1, 2 -> 2)
    val multimap = list.toBagMultimap

    multimap must haveTheSameElementsAs(Map(1 -> Bag(1, 1, 2), 2 -> Bag(2)))

    val extended = (multimap.toSeq :+ (1 -> Bag(2, 3, 4, 5)))
    val merged = extended.mergeKeys(_ ++ _)

    merged must haveTheSameElementsAs(Map(1 -> Bag(1, 1, 2, 2, 3, 4, 5), 2 -> Bag(2)))
  }
}
