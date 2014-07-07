package uk.ac.ed.inf.mois

abstract class Scheduler {
  def init(group: ProcessGroup) {}
  def apply(t: Double, tau: Double, group: ProcessGroup): Double
}
