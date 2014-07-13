package uk.ac.ed.inf.mois.sched

import scala.collection.mutable
import uk.ac.ed.inf.mois.{DoubleVar, BaseProcess, ProcessGroup, VarMap, VarConversions}

class KarrScheduler(step: Double) extends MapReduceScheduler(step) with VarConversions {

  type Acc = VarMap[Double, DoubleVar]

  var totalDemand: Acc = null
  val demand = mutable.Map.empty[BaseProcess, Acc]
  var first = true // first run
  var dt0 = step

  val accumulator = new Acc

  private def zeroAccumulator(group: ProcessGroup) {
    if (accumulator.size == 0) {
      for ((m, v) <- group.doubleVars) {
	accumulator(m) = (v.copy := 0.0)
      }
    } else {
      for (v <- accumulator.values) {
	v := 0.0
      }
    }
  }

  override def init(group: ProcessGroup) {
    if (demand.size == 0) {
      for (p <- group.processes) {
	demand(p) = new Acc
	for ((m, v) <- p.doubleVars) {
	  demand(p)(m) = v.copy
	}
      }
    }
  }

  override def before(t: Double, dt: Double, acc: Acc, group: ProcessGroup) = {
    // zero the accumulator
    zeroAccumulator(group)
    if (first) dt
    // this is where we cleverly figure out a better timestep
    else dt0
  }

  override def after(t: Double, dt: Double, acc: Acc, group: ProcessGroup) = {
    //println(s"after($t, $dt, $first)")
    totalDemand = acc.copy
    if (first) {
      first = false
      0
    } else {
      // update the process group's state
      // this should be written as
      // group.doubleVars <<< totalDemand map(_ * dt)
      for ((m, v) <- totalDemand)
        group.doubleVars(m) := v * dt
      dt
    }
  }

  /** map function */
  def m(t: Double, dt: Double, group: ProcessGroup, proc: BaseProcess) = {
    //println(s"m($t, $dt, $first)")
    if (first) {
      // just give the process everything so we see what its demand looks like
      group >>> proc
    } else {
      // we have a total demand, so give this process its portion
      for ((m, v) <- proc.doubleVars) {
	if (totalDemand(m).value != 0.0) {
          v := group.doubleVars(m) * demand(proc)(m) / totalDemand(m)
	} else {
	  v := group.doubleVars(m)
	}
      }
    }

    val s0 = proc.doubleVars.copy
    //println(s"m --> ${s0.toSeq}")
    proc(t, dt)
    //println(s"m <-- ${proc.doubleVars.toSeq}")
    val s1 = proc.doubleVars // just for clarity

    // fill in demand per unit time for our variables
    for ((m, _) <- s0) {
      val dvdt = (s1(m) - s0(m)) / dt
      // demand means a negative gradient
      if (dvdt < 0)
	demand(proc)(m) := dvdt
      else
	demand(proc)(m) := 0.0
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

  /** reduce function */
  def r(acc: Acc, proc: BaseProcess) = {
    //println(s"r($first) $proc ${proc.allVars}")
    // sum up total demand for this iteration in the accumulator
    for ((m, v) <- proc.doubleVars)
      acc(m) += demand(proc)(m).copy
    acc
  }
}
