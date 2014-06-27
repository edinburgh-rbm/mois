package ed.mois

abstract class Process(val name: String) {
  var state = new State

  /*
   * Helper function used in "preamble" to declare a resource
   * variable and add it to this process' state table
   */ 
  def resource[T](value: T, identifier: String, scope: String = "default"): Resource[T] = {
    val p = new Resource[T](value, identifier, scope)
    state += p
    p
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
    val start = state.deepCopy
    step(t, tau)
    state - start
  }

  override def toString = name + state.toString
}  

