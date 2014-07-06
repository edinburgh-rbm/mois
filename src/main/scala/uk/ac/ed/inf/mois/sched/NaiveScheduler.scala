package uk.ac.ed.inf.mois.sched

import scala.util.Random

import uk.ac.ed.inf.mois.{ProcessGroup, Scheduler}

class NaiveScheduler(step: Double) extends Scheduler {
  def apply(t: Double, tau: Double, group: ProcessGroup) {
    var dt = 0.0
    while (dt < tau) {
      for (child <- Random.shuffle(group.processes)) {
	group >>> child
	child.step(t + dt, step)
	group <<< child
      }
      dt += step
    }
  }
}
