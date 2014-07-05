package uk.ac.ed.inf.mois.sched

import scala.util.Random

import uk.ac.ed.inf.mois.{Process, State, Scheduler}

class NaiveScheduler(step: Double) extends Scheduler {
  def apply(t: Double, tau: Double, state: State, procs: Process*) {
    var dt = 0.0
    while (dt < tau) {
      for (proc <- Random.shuffle(procs)) {
	proc.step(t + dt, step)
      }
      dt += step
    }
  }
}
