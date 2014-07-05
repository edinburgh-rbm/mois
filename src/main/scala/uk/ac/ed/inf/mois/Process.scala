package uk.ac.ed.inf.mois

import scala.collection.mutable

/** A `Process` is basically a `State` and a function that operates
  * upon it parametrised by time.
  */
abstract class Process(val name: String) extends VarContainer {

  implicit def getVarValue[T](v: Var[T]) = v.value

  def state: mutable.ArrayBuffer[Var[_]] = {
    val s = mutable.ArrayBuffer.empty[Var[_]]
    for (v <- ints) s += v
    for (v <- longs) s += v
    for (v <- floats) s += v
    for (v <- bools) s += v
    s
  }

  val stepHandlers = mutable.ArrayBuffer.empty[StepHandler]

  def addStepHandler(sh: StepHandler) {
    stepHandlers += sh
  }
  
  /** This function takes the state from where it is at
    * time t to where it is at t + tau. This must be supplied
    * by concrete sub-classes.
    */
  def step(t: Double, tau: Double) 

  /** A wrapper around the user defined step function to calculate
    * changes.
    */
  def apply(t: Double, tau: Double) { //: State = {
    // val start = state.copy
    step(t, tau)
    for (sh <- stepHandlers)
      sh.handleStep(t+tau, this)
    // state - start
  }

  def stringPrefix = "Process"
  override def toString = stringPrefix + "(" + name + ")"
}  

