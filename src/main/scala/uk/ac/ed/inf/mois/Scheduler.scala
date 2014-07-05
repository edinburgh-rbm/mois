package uk.ac.ed.inf.mois

abstract class Scheduler {
  def apply(t: Double, tau: Double, parent: Process, children: Process*)
}
