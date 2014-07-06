package uk.ac.ed.inf.mois

import scala.reflect.{classTag, ClassTag}

import scala.collection.mutable

/*
 * A `ProcessGroup` is a list of `Process`es and a `Scheduler`. It presents the
 * same interface as a `Process` and so hierarchies of them can be built.
 */
class ProcessGroup(name: String) extends Process(name) {

  var processes = mutable.ArrayBuffer.empty[(Process,
    Array[(Var[T], Var[T]) forSome { type T }])]
  var scheduler: Scheduler = null

  /** The += operator adds a process to the group. */
  def +=(proc: Process) = {
    val varlist = mutable.ArrayBuffer.empty[(Var[T], Var[T]) forSome { type T }]

    def add[V <: Var[_] { type R = V }: ClassTag](
      procVars: mutable.ArrayBuffer[V],
      thisVars: mutable.ArrayBuffer[V]) {
      for (v <- procVars) {
	val myv: V =
          if (vmap contains v.meta) {
            vmap(v.meta) match {
              case v: V => v
              case _ => throw new IllegalArgumentException(
                "adding variable " + v + " of type " +
                classTag[V].runtimeClass.getCanonicalName() +
                " is not possible because this container has a " +
                " variable with the same meta but a different type")
            }
          } else {
	    val vcopy = v.copy
	    vmap += v.meta -> vcopy
	    thisVars += vcopy
	    vcopy
	  }
	varlist += ((v, myv).asInstanceOf[(Var[T], Var[T]) forSome { type T }])
      }
    }

    add(proc.ints, this.ints)
    add(proc.longs, this.longs)
    add(proc.floats, this.floats)
    add(proc.doubles, this.doubles)
    add(proc.bools, this.bools)

    processes += (proc -> varlist.toArray)

    this
  }

  /** The -= operator removes a process from the group. */
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
