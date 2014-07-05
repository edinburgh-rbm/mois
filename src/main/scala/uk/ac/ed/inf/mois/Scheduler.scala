package uk.ac.ed.inf.mois

abstract class Scheduler {
  def apply(t: Double, tau: Double, state: State, procs: Process*)
}
