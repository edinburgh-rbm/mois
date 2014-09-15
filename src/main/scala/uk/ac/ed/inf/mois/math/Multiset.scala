package uk.ac.ed.inf.mois.math

import spire.algebra.{AdditiveMonoid, Eq}

class Multiset[T] private (val members: Map[T, Int])
    extends Map[T, Int] {

  override def toString = (for ((m, c) <- members) yield
    c + "*" + m).mkString(" + ")
  // -- Map methods --
  def get(m: T) = members get m
  def iterator = members.iterator
  def + [A >: Int](kv: (T, A)) = {
    val (m, x) = kv
    x match {
      case i: Int => members get m match {
        case Some(j) => new Multiset(members + (m -> (i+j)))
        case None => new Multiset(members + (m -> i))
      }
      case _ => members + kv
    }
  }
  def - (m: T) = members get m match {
    case Some(i) =>
      if (i == 1) new Multiset(members - m)
      else new Multiset(members + (m -> (i-1)))
    case None => members
  }
  override def default(m: T) = 0
  override def foreach[U](f: ((T, Int)) => U) = members foreach f
  override def empty: Multiset[T] = Multiset()
  override def size = members.size
  def multisize: Int = members.values.sum
  def multiseq: Seq[T] =
    (for ((m, n) <- members; _ <- 1 to n) yield m).toSeq

  // -- Multiset creation methods --
  // some reason + appears to be duplicated (above) with only
  // slightly different type bounds?
  def + (kv: (T, Int)): Multiset[T] = {
    val (m, i) = kv
    members get m match {
      case Some(j) => new Multiset(members + (m -> (i+j)))
      case None => new Multiset(members + kv)
    }
  }
  def + (m: T): Multiset[T] = this + (m, 1)
  def + (that: Multiset[T]): Multiset[T] =
    if (this.size >= that.size)
      that.foldLeft(this)({ case (m, kv) => m + kv })
    else
      this.foldLeft(that)({ case (m, kv) => m + kv })
  def * (n: Int) = (0 until (n-1)).foldLeft(
    this)({ case (m, _) => m + this })
}

object Multiset {
  def apply[T](m: (T, Int), ms: (T, Int)*): Multiset[T] =
    ms.foldLeft(empty[T] + m)({ case (z, s) => z + s })
  def apply[T](ms: T*) = new Multiset(
    ms.groupBy(x => x).map({ case (s, ss) => (s, ss.size) }))
  def empty[T] = new Multiset[T](Map.empty[T, Int])
}

final class MultisetAdditive[T] extends AdditiveMonoid[Multiset[T]] {
  def zero = Multiset.empty[T]
  def plus(x: Multiset[T], y: Multiset[T]) = x + y
}

final class MultisetEq[T](implicit eq: Eq[T], meq: Eq[Map[T, Int]] )
    extends Eq[Multiset[T]] {
  def eqv(x: Multiset[T], y: Multiset[T]) = {
    println(s"XXXXXX ${x.members}")
    println(s"CCCCCC ${y.members}")
    for ( (u, v) <- x.members.keys zip y.members.keys ) {
      println(s"UVUVU $u $v ${Eq[T].eqv(u, v)}")
    }
    import spire.std.int._
    for ( (u, v) <- x.members.values zip y.members.values ) {
      println(s"WWWWWW $u $v ${Eq[Int].eqv(u, v)}")
    }
    meq.eqv(x.members, y.members)
  }
}

trait MultisetInstances {
  import spire.implicits._
  implicit def MultisetAdditive[T] = new MultisetAdditive[T]
  implicit def MultisetEq[T: Eq] = new MultisetEq[T]
}
