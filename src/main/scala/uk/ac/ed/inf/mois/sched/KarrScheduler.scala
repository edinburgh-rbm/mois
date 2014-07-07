package uk.ac.ed.inf.mois.sched

import scala.collection.mutable
import uk.ac.ed.inf.mois.{MapReduceScheduler, NumericVar, Process, ProcessGroup, VarMap}
import uk.ac.ed.inf.mois.VarConv._

class KarrScheduler extends MapReduceScheduler {
  type ACC = VarMap[Double, NumericVar[Double]]

  var totalDemand: ACC = null
  val demand = mutable.Map.empty[Process, ACC]
  var first = true // first run
  var tau0 = 0.0

  def accumulator = new ACC


  override def before(t: Double, tau: Double, acc: ACC, group: ProcessGroup) = {
    println(s"before($t, $tau, $first)")
    if (first) tau
    else {
      // this is where we cleverly figure out a better timestep
      tau0
    }
  }

  override def after(t: Double, tau: Double, acc: ACC, group: ProcessGroup) = {
    println(s"after($t, $tau, $first)")
    totalDemand = acc
    if (first) {
      first = false
      tau0 = tau
      0
    } else {
      // update the process group's state
      for (v <- totalDemand)
        group.doubleVars(v) := v * tau
      tau
    }
  }

  /**
   * map function
   */
  def m(t: Double, dt: Double, group: ProcessGroup, proc: Process) = {
    println(s"m($t, $dt, $first)")
    if (first) {
      // we need an entry in the demand table for this process
      demand += proc -> new ACC
    } else {
      // we have a total demand, so give this process its portion
      for (v <- proc.doubleVars) {
	if (totalDemand(v).value != 0.0) {
          v := group.doubleVars(v) * demand(proc)(v) / totalDemand(v)
	} else {
	  v := group.doubleVars(v)
	}
      }
    }

    val s0 = proc.doubleVars.copy
    println(s"m --> ${s0.toSeq}")
    proc(t, dt)
    println(s"m <-- ${proc.doubleVars.toSeq}")
    val s1 = proc.doubleVars // just for clarity

    // fill in demand per unit time for our variables
    for (v <- s0) {
      if (!(demand(proc) contains v))
	demand(proc) << v.copy
      demand(proc)(v) := (s1(v) - s0(v)) / dt
    }

    // return the process itself to send to the reduce function
    proc
  }

  /**
   * reduce function
   */
  def r(acc: ACC, proc: Process) = {
    println(s"r($first) $proc ${proc.allVars}")
    // sum up total demand for this iteration in the accumulator
    for (v <- proc.doubleVars)
      if (acc contains v)
	acc(v) += demand(proc)(v)
      else
	acc << demand(proc)(v).copy
    acc
  }
}
