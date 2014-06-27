package ed.mois

import org.apache.commons.math3.ode.{FirstOrderIntegrator, FirstOrderDifferentialEquations}
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator

abstract class ProcessODE(name: String) extends Process(name) with FirstOrderDifferentialEquations {
  // The integrator object which can be any implementation compatible
  // with the Apache Commons Math ODE library. Free to override in 
  // subclasses. By default we use the Dormand Prince 8,5,3 integrator
  // from their example documentation.
  val integrator: () => FirstOrderIntegrator = () => new DormandPrince853Integrator(1.0e-8, 100.0, 1.0e-10, 1.0e-10)

  // An array holding resources of type Double that are used by the
  // ODE solver.
  var y: Array[Resource[Double]] = null

  // This method must be called once and only once to set the list of
  // resources that are going to be integrated by the ODE solver. It
  // populates the `y` array.
  def integral(ys: Resource[Double]*) {
    assert(y eq null)
    y = ys.toArray
  }

  def step(t: Double, tau: Double) {
    // unify integral state with process state. this is right now a necessary
    // evil because process state may have changed and it won't have changed
    // our array of resource references used for the integration
    for ( i <- 0 to (y.size - 1) )
      y(i) = state(y(i)).asInstanceOf[Resource[Double]]

    // construct array of doubles corresponding to the integral resources
    // which is what the ODE solver will actually use
    val doubleY = (for (resY <- y) yield resY.value).toArray

    // conduct the integration
    integrator().integrate(this, t, doubleY, t+tau, doubleY)

    // put the results of the integration into the resources
    for ( i <- 0 to (y.size - 1) )
      y(i) := doubleY(i)
  }

  // This is required by the ODE solver and gives the dimension of
  // the vector-valued integral.
  def getDimension = y.size

  // This is the method that does the actual work. It must be implemented
  // by concrete sub-classes which should calculate the values of the
  // derivatives (third argument, ẏ) in terms of the (given) first two.
  // Indexes into the arrays are as given in the original call to the
  // `integral` method and as used with the class' `y` attribute.
  def computeDerivatives(t: Double, y: Array[Double], ẏ: Array[Double])
}
