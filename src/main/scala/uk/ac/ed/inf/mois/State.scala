package uk.ac.ed.inf.mois

import scala.collection.mutable.Map

class State {
  val table = Map[VarKey, Var[_]]()

  def apply[T](v: Var[_]) = table.apply(v.key).asInstanceOf[Var[T]]
  def apply[T](k: VarKey) = table.apply(k).asInstanceOf[Var[T]]

  def filter = table.filter _

  def +=(v: Var[_]) = {
    table += v.key -> v
  }

  def ++=(s: State) = {
    for ((_,v) <- s.table)
      this += v
    this
  }

  def -(s: State) = {
    val ns = new State
    for ((k,v) <- table) {
      if (s.table contains k) {
	ns += v - s(v)
      } else {
	ns += v
      }
    }
    for ((k,v) <- s.table) {
      if (!(table contains k)) {
	ns += v - v
      }
    }
    ns
  }

  override def toString = "(" + (for ((_,v) <- table) yield v).mkString(", ") + ")"

  def deepCopy = {
    val ns = new State
    for ((_, v) <- table) {
      ns += v.clone
    }
    ns
  }
}
