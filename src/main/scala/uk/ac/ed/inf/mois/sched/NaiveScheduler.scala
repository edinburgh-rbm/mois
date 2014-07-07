package uk.ac.ed.inf.mois.sched

import scala.util.Random

import uk.ac.ed.inf.mois.{ProcessGroup, Scheduler}

class NaiveScheduler(step: Double) extends Scheduler {
  def apply(t: Double, tau: Double, group: ProcessGroup) = {
    for (child <- Random.shuffle(group.processes)) {
      group >>> child
      child.step(t, step)
      group <<< child
    }
    step
  }
}
