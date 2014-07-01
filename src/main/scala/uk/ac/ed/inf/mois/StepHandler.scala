package uk.ac.ed.inf.mois

import scala.collection.mutable.Map

abstract class StepHandler {
  def handleStep(t: Double, state: State)
}

class Accumulator extends StepHandler {
  var history = Map.empty[Double, State]
  def handleStep(t: Double, state: State) {
    history += t -> state.copy
  }
  def apply(t: Double) = history(t)
}
