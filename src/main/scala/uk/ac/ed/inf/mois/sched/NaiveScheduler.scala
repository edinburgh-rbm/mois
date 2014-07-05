package uk.ac.ed.inf.mois.sched

import scala.util.Random

import uk.ac.ed.inf.mois.{ProcessGroup, Scheduler}

class NaiveScheduler(step: Double) extends Scheduler {
  def apply(t: Double, tau: Double, group: ProcessGroup) {
    var dt = 0.0
    while (dt < tau) {
      for ((child, varmap) <- Random.shuffle(group.processes)) {
	for ((childVar, groupVar) <- varmap)
	  childVar := groupVar.value.asInstanceOf[childVar.Val]
	child.step(t + dt, step)
	for ((childVar, groupVar) <- varmap)
	  groupVar := childVar.value.asInstanceOf[groupVar.Val]
      }
      dt += step
    }
  }
}
