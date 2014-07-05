package uk.ac.ed.inf.mois.sched

import scala.util.Random

import uk.ac.ed.inf.mois.{Process, Scheduler}

class NaiveScheduler(step: Double) extends Scheduler {
  def apply(t: Double, tau: Double, parent: Process, children: Process*) {
    var dt = 0.0
    while (dt < tau) {
      for (child <- Random.shuffle(children)) {
	child.step(t + dt, step)
      }
      dt += step
    }
  }
}
