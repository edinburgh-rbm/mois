package uk.ac.ed.inf.mois

import scala.collection.mutable

/** A `Process` is basically a `State` and a function that operates
  * upon it parametrised by time.
  */
abstract class Process(val name: String) {

  implicit def getVarValue[T](v: Var[T]) = v.value

  val ints = mutable.ArrayBuffer.empty[NumericVar[Int]]
  val longs = mutable.ArrayBuffer.empty[NumericVar[Long]]
  val floats = mutable.ArrayBuffer.empty[NumericVar[Float]]
  val doubles = mutable.ArrayBuffer.empty[NumericVar[Double]]
  val bools = mutable.ArrayBuffer.empty[BooleanVar]

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
      sh.handleStep(t+tau, state)
    // state - start
  }

  def stringPrefix = "Process"
  override def toString = stringPrefix + "(" + name + ")"
}  

