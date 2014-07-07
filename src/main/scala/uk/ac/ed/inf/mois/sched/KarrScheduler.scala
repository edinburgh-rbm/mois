package uk.ac.ed.inf.mois.sched

import scala.collection.mutable
import uk.ac.ed.inf.mois.{NumericVar, Process, ProcessGroup, VarMap}
import uk.ac.ed.inf.mois.VarConv._

class KarrScheduler(step: Double) extends MapReduceScheduler(step) {
  type ACC = VarMap[Double, NumericVar[Double]]

  var totalDemand: ACC = null
  val demand = mutable.Map.empty[Process, ACC]
  var first = true // first run
  var dt0 = step

  val accumulator = new ACC

  private def zeroAccumulator(group: ProcessGroup) {
    if (accumulator.size == 0) {
      for (v <- group.doubleVars) {
	accumulator += v.copy := 0.0
      }
    } else {
      for (v <- accumulator) {
	v := 0.0
      }
    }
  }

  override def init(group: ProcessGroup) {
    if (demand.size == 0) {
      for (p <- group.processes) {
	demand(p) = new ACC
	for (v <- p.doubleVars) {
	  demand(p) << v.copy
	}
      }
    }
  }

  override def before(t: Double, dt: Double, acc: ACC, group: ProcessGroup) = {
    // zero the accumulator
    zeroAccumulator(group)
    if (first) dt
    else {
      // this is where we cleverly figure out a better timestep
      dt0
    }
  }

  override def after(t: Double, dt: Double, acc: ACC, group: ProcessGroup) = {
    //println(s"after($t, $dt, $first)")
    totalDemand = acc.copy
    if (first) {
      first = false
      0
    } else {
      // update the process group's state
      // this should be written as
      //    group.doubleVars <<< totalDemand map(_ * dt)
      for (v <- totalDemand)
        group.doubleVars(v) := v * dt
      dt
    }
  }

  /**
   * map function
   */
  def m(t: Double, dt: Double, group: ProcessGroup, proc: Process) = {
    //println(s"m($t, $dt, $first)")
    if (first) {
      // just give the process everything so we see what its demand looks like
      group >>> proc
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
    //println(s"m --> ${s0.toSeq}")
    proc(t, dt)
    //println(s"m <-- ${proc.doubleVars.toSeq}")
    val s1 = proc.doubleVars // just for clarity

    // fill in demand per unit time for our variables
    for (v <- s0) {
      val dvdt = (s1(v) - s0(v)) / dt
      // demand means a negative gradient
      if (dvdt < 0)
	demand(proc)(v) := dvdt
      else
	demand(proc)(v) := 0.0
    }

/*
    println(s"$t $proc")
    println(s"    -> $s0")
    println(s"    <- $s1")
    println(s"    <> ${demand(proc)}")
*/
    // return the process itself to send to the reduce function
    proc
  }

  /**
   * reduce function
   */
  def r(acc: ACC, proc: Process) = {
    //println(s"r($first) $proc ${proc.allVars}")
    // sum up total demand for this iteration in the accumulator
    for (v <- proc.doubleVars)
      acc(v) += demand(proc)(v).copy
    acc
  }
}
