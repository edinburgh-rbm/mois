package uk.ac.ed.inf.mois

import scala.reflect.ClassTag

import scala.collection.mutable

/*
 * A `ProcessGroup` is a list of `Process`es and a `Scheduler`. It presents the
 * same interface as a `Process` and so hierarchies of them can be built.
 */
class ProcessGroup(name: String) extends Process(name) {
  var processes = mutable.ArrayBuffer.empty[(Process, Array[(Var[T], Var[T]) forSome { type T }])]
  var scheduler: Scheduler = null

  val vmap = mutable.Map.empty[VarMeta, Var[_]]

  /*
   * The += operator adds a process to the group
   */
  def +=(proc: Process) = {
    val varlist = mutable.ArrayBuffer.empty[(Var[T], Var[T]) forSome { type T }]

    def add[V <: Var[_]: ClassTag](pv: mutable.ArrayBuffer[V], pgv: mutable.ArrayBuffer[V]) = {
      def f(v: V) = {
	val myv: V = if (!(vmap contains v.meta)) {
	  val vcopy = v.copy.asInstanceOf[V]
	  vmap += v.meta -> vcopy
	  pgv += vcopy
	  vcopy
	} else {
	  vmap(v.meta) match {
	    case v: V => v
	    case _ => throw new IllegalArgumentException("bad")
	  }
	}
	varlist += ((v, myv).asInstanceOf[(Var[T], Var[T]) forSome { type T }])
      }
      pv map (v => f(v))
    }

    add(proc.ints, this.ints)
    add(proc.longs, this.longs)
    add(proc.floats, this.floats)
    add(proc.doubles, this.doubles)
    add(proc.bools, this.bools)

    processes += (proc -> varlist.toArray)

    this
  }

  /*
   * The -= operator removes a process from the group
   */
  def -=(proc: Process) = {
    // TODO: needed for process migration. Keeping state
    // coherent is important here
    this
  }

  /*
   * The `step` method of the `Process` interface calls the `Scheduler` on
   * the listof processes together with the group state table and time
   * parameters
   */
  def step(t: Double, tau: Double) {
    scheduler(t, tau, this)
  }
}
