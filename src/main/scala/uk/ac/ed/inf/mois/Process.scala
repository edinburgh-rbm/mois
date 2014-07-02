package uk.ac.ed.inf.mois

import uk.ac.ed.inf.mois.{Var => V}
import uk.ac.ed.inf.mois.Conversions._

/*
 * A `Process` is basically a `State` and a function that operates
 * upon it parametrised by time.
 */
abstract class Process(val name: String) {
  var state = new State

  /*
   * Helper function used in "preamble" to declare a variable
   * variable and add it to this process' state table. It returns
   * not the variable itself but a function that pulls it out of
   * the state table because this is necessary to merge references
   * from different processes that share some state
   */ 
  def Var[T](value: T, identifier: String, scope: String = "default"): VarH[T] = {
    val v: Var[_] = value match {
      case x: Int => Var(x, identifier, scope)
      case x: Long => Var(x, identifier, scope)
      case x: Float => Var(x, identifier, scope)
      case x: Double => Var(x, identifier, scope)
      case b: Boolean => new BooleanVar(b, identifier, scope)
      case _ => throw new IllegalArgumentException(
        "I don't know how to handle this type")
    }
    if (!(state contains v))
      state += v
    new VarH[T] {
      def apply() = state(v.key)
    }
  }

  /*
   * This function takes the state from where it is at
   * time t to where it is at t + tau. This must be supplied
   * by concrete sub-classes.
   */ 
  def step(t: Double, tau: Double) 

  /*
   * A wrapper around the user defined step function to calculate changes
   */
  def apply(t: Double, tau: Double): State = {
    val start = state.copy
    step(t, tau)
    state - start
  }

  override def toString = name + state.toString
}  

