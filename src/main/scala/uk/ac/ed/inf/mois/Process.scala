package uk.ac.ed.inf.mois

import uk.ac.ed.inf.mois.{Var => V}
import uk.ac.ed.inf.mois.Conversions._

abstract class Process(val name: String) {
  var state = new State

  /*
   * Helper function used in "preamble" to declare a resource
   * variable and add it to this process' state table. It returns
   * not the variable itself but a function that pulls it out of
   * the state table because this is necessary to merge references
   * from different processes that share some state
   */ 
  def Var[T](value: T, identifier: String, scope: String = "default"): VarH[T] = {
    val p = V[T](value, identifier, scope)
    if (!(state contains p))
      state += p
    object vp extends VarH[T] {
      def apply() = state(p.key)
    }
    vp
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

