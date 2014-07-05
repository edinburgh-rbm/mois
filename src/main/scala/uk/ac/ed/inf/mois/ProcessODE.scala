package uk.ac.ed.inf.mois

import org.apache.commons.math3.ode
import org.apache.commons.math3.ode.sampling
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator

/**
 * A partial implementation of `Process` that uses the Apache Commons
 * Math ODE library to implement its `step` method. The `computeDerivatives`
 * method must be filled out to describe the system of differential equations
 */
abstract class ProcessODE(name: String) extends Process(name) with ode.FirstOrderDifferentialEquations {
  /**
   * The integrator object which can be any implementation compatible
   * with the Apache Commons Math ODE library. Free to override in 
   * subclasses. By default we use the Dormand Prince 8,5,3 integrator
   * from their example documentation.
   */
  val integrator: () => ode.FirstOrderIntegrator = () => new DormandPrince853Integrator(1.0e-8, 100.0, 1.0e-10, 1.0e-10)

  /**
   * An array holding variables of type Double that are used by the
   * ODE solver.
   */
  var y: Array[VarH[Double]] = null

  /**
   * This method must be called once and only once to set the list of
   * variables that are going to be integrated by the ODE solver. It
   * populates the `y` array.
   */
  def integral(ys: VarH[Double]*) {
    assert(y eq null)
    y = ys.toArray
  }

  /**
   * main function implementing the `Process` interface
   */
  def step(t: Double, tau: Double) {
    // construct array of doubles corresponding to the integral variables
    // which is what the ODE solver will actually use
    val doubleY = (for (resY <- y) yield resY().value).toArray

    // construct the integrator
    val i = integrator()
   
    // only add step handlers if we have them
    if (stepHandlers.size > 0) {
      object SH extends sampling.StepHandler {
	def init(t0: Double, y0: Array[Double], t: Double) {}
	def handleStep(interp: sampling.StepInterpolator, isLast: Boolean) {
	  val t = interp.getCurrentTime()
	  val ydot = interp.getInterpolatedState()
	  for (i <- 0 until y.size)
	    y(i)() := ydot(i)
	  for (sh <- stepHandlers)
	    sh.handleStep(t, state)
	}
      }
      i.addStepHandler(SH)
    }

    // conduct the integration
    i.integrate(this, t, doubleY, t+tau, doubleY)

    // put the results of the integration into the variables
    for (i <- 0 until y.size)
      y(i)() := doubleY(i)
  }

  /**
   * Override `Process`'  wrapper around the user defined step function to
   * calculate changes because we have our own way of running step
   * handlers inside apache commons' integrator
   */
  override def apply(t: Double, tau: Double): State = {
    val start = state.copy
    step(t, tau)
    state - start
  }

  /**
   * This is required by the ODE solver and gives the dimension of
   * the vector-valued integral.
   */
  def getDimension = y.size

  /**
   * This is the method that does the actual work. It must be implemented
   * by concrete sub-classes which should calculate the values of the
   * derivatives (third argument, ẏ) in terms of the (given) first two.
   * Indexes into the arrays are as given in the original call to the
   * `integral` method and as used with the class' `y` attribute.
   */
  def computeDerivatives(t: Double, y: Array[Double], ydot: Array[Double])
}
