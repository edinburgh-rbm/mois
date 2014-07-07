package uk.ac.ed.inf.mois

abstract class Scheduler {
  def apply(t: Double, tau: Double, group: ProcessGroup)
}

abstract class MapReduceScheduler extends Scheduler {
  type ACC

  def accumulator: ACC

  def m(t: Double, tau: Double, group: ProcessGroup, proc: Process): Process
  def r(acc: ACC, proc: Process): ACC

  def before(t: Double, tau: Double, acc: ACC, group: ProcessGroup) = tau
  def after(t: Double, tau: Double, acc: ACC, group: ProcessGroup) = tau

  def apply(t0: Double, tau: Double, group: ProcessGroup) {
    var t = t0
    var dt = tau
    while (t < t0+tau) {
      val initial = accumulator
      dt = before(t, dt, initial, group)
      val procs = group.processes map (m(t, dt, group, _)) 
      val results = procs.foldLeft(initial) (r _)
      dt = after(t, dt, results, group)
      t += dt
    }
  }
}
