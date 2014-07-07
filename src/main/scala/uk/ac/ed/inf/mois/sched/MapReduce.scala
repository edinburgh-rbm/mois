package uk.ac.ed.inf.mois.sched

import scala.util.Random
import java.lang.Math.min

import uk.ac.ed.inf.mois.{Scheduler, Process, ProcessGroup}

abstract class MapReduceScheduler(step: Double) extends Scheduler {
  type ACC

  def accumulator: ACC

  def m(t: Double, dt: Double, group: ProcessGroup, proc: Process): Process
  def r(acc: ACC, proc: Process): ACC
  
  def before(t: Double, dt: Double, acc: ACC, group: ProcessGroup) = dt
  def after(t: Double, dt: Double, acc: ACC, group: ProcessGroup) = dt

  def apply(t: Double, tau: Double, group: ProcessGroup) = {
    val acc = accumulator
    val dt = before(t, min(tau, step), acc, group)
    val procs = Random.shuffle(group.processes) map (m(t, dt, group, _)) 
    val results = procs.foldLeft(acc) (r _)
    after(t, dt, results, group)
  }
}
