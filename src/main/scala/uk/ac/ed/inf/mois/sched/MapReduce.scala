package uk.ac.ed.inf.mois.sched

import scala.util.Random
import java.lang.Math.min

import uk.ac.ed.inf.mois.{Scheduler, BaseProcess, ProcessGroup}

abstract class MapReduceScheduler(step: Double) extends Scheduler {

  type Acc

  def accumulator: Acc

  def m(t: Double, dt: Double, group: ProcessGroup, proc: BaseProcess): BaseProcess
  def r(acc: Acc, proc: BaseProcess): Acc
  
  def before(t: Double, dt: Double, acc: Acc, group: ProcessGroup) = dt
  def after(t: Double, dt: Double, acc: Acc, group: ProcessGroup) = dt

  def apply(t: Double, tau: Double, group: ProcessGroup) = {
    val acc = accumulator
    val dt = before(t, min(tau, step), acc, group)
    val procs = Random.shuffle(group.processes) map (m(t, dt, group, _)) 
    val results = procs.foldLeft(acc) (r _)
    after(t, dt, results, group)
  }
}
