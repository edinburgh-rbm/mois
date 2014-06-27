package ed.mois

import scala.collection.mutable.Map

class State {
  val table = Map[(String, String), Resource[_]]()
  def apply[T](r: Resource[_]) = table.apply((r.identifier, r.scope)).asInstanceOf[Resource[T]]
  def apply[T](i: (String, String)) = table.apply(i).asInstanceOf[Resource[T]]

  def filter = table.filter _

  def +=(r: Resource[_]) = {
    table update ((r.identifier, r.scope), r)
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
