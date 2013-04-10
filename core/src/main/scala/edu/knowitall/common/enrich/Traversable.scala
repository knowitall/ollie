package edu.knowitall
package common
package enrich

import edu.knowitall.collection.immutable.Bag

import scalaz._
import Scalaz._

/**
 * Enrichments for traversables.
 *
 * @author  Michael Schmitz
 */
object Traversables {
  implicit def traversableOnceTo[T](as: TraversableOnce[T]): SuperTraversableOnce[T] = new SuperTraversableOnce[T] {
    val value = as
  }

  implicit def traversableOncePairIntTo[T](as: TraversableOnce[(T, Int)]): SuperTraversableOncePairInt[T] = new SuperTraversableOncePairInt[T] {
    val value = as
  }

  implicit def traversableOncePairTo[T, U](as: TraversableOnce[(T, U)]): SuperTraversableOncePair[T, U] = new SuperTraversableOncePair[T, U] {
    val value = as
  }
}

sealed trait SuperTraversableOnce[T] extends scalaz.PimpedType[TraversableOnce[T]] {
  def histogram: Map[T, Int] = {
    value.foldLeft(Map[T, Int]()) { (m, c) =>
      m.updated(c, m.getOrElse(c, 0) + 1)
    }
  }
}

sealed trait SuperTraversableOncePairInt[T] extends scalaz.PimpedType[TraversableOnce[(T, Int)]] {
  import Traversables._
  def mergeHistograms: Map[T, Int] = value.mergeKeys(_ + _)
}

sealed trait SuperTraversableOncePair[T, U] extends scalaz.PimpedType[TraversableOnce[(T, U)]] {
  def mergeKeys(implicit mon: Semigroup[U]): Map[T, U] = {
    value.foldLeft(Map[T, U]()) {
      case (map, (k, v)) =>
        map + (k -> (map.get(k).map(_ |+| v).getOrElse(v)))
    }
  }

  def mergeKeys[F[_]](implicit point: Pointed[F], sem: Semigroup[F[U]]): Map[T, F[U]] = {
    value.foldLeft(Map[T, F[U]]()) {
      case (map, (k, v)) =>
        val pure = v.pure[F]
        map + (k -> (map.get(k).map(_ |+| pure).getOrElse(pure)))
    }
  }

  def mergeKeys(merge: (U, U) => U): Map[T, U] = {
    value.foldLeft(Map[T, U]()) {
      case (map, (k, v)) =>
        map + (k -> map.get(k).map(merge(_, v)).getOrElse(v))
    }
  }

  def toListMultimap: Map[T, List[U]] = {
    value.foldLeft(Map[T, List[U]]().withDefaultValue(List.empty[U])) {
      case (map, (k, v)) =>
        map + (k -> (v :: map(k)))
    }
  }

  def toSetMultimap: Map[T, Set[U]] = {
    value.foldLeft(Map[T, Set[U]]().withDefaultValue(Set.empty[U])) {
      case (map, (k, v)) =>
        map + (k -> (map(k) + v))
    }
  }

  def toBagMultimap: Map[T, Bag[U]] = {
    value.foldLeft(Map[T, Bag[U]]().withDefaultValue(Bag.empty[U])) {
      case (map, (k, v)) =>
        val bag = map(k)
        map + (k -> (bag + v))
    }
  }
}
