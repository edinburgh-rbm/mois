/*
 *  MOIS: Ordinary Differential Equation Process
 *  Copyright (C) 2014 University of Edinburgh School of Informatics
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package uk.ac.ed.inf.mois

import language.implicitConversions

import org.apache.commons.math3.ode
import org.apache.commons.math3.ode.sampling
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator

import collection.mutable

class ODEDebugHandler extends sampling.StepHandler {
  def init(t0: Double, y0: Array[Double], t: Double) {
    println(s"ODE Debug init: t0: ${t0} t: ${t} y0: ${y0.toSeq}")
  }
  
  def handleStep(interpolator: sampling.StepInterpolator, isLast: Boolean) {
    val t = interpolator.getCurrentTime();
    val y = interpolator.getInterpolatedState();
    println(s"ODE Debug: ${t}\t${y.toSeq}")
  }
}

/** A partial implementation of `Process` that uses the Apache Commons
  * Math ODE library to implement its `step` method.
  */
abstract class ODE extends Process
    with ode.FirstOrderDifferentialEquations {

  type Derivative = () => Double

  /** Functions defining the derivatives of the variables in `vars`.
    * The two arrays are indexed equally.
    */
  val funs = mutable.ArrayBuffer.empty[Derivative]

  /** An array with all `Var`s for which to integrate. */
  val vars = mutable.ArrayBuffer.empty[Var[Double]]

  /** A class to define derivatives of `Var`s. */
  protected class AddODE(val vs: Seq[Var[Double]]) {

    /** Adds an ODE definition to the process. */
    def := (fs: Derivative*): Unit = {
      require(fs.size == vs.size,
        "lhs and rhs of ODE system must have same size")
      for ((v, f) <- vs zip fs) {
        vars += v
        funs += f
      }
    }
  }
  implicit def bynameToFun(f: => Double) = () => f
  implicit def varToFun(f: Var[Double]) = () => f.value

  /** Adds an ODE definition to the current `ODE`. */
  protected def d(vs: Var[Double]*) = new AddODE(vs) {
    def / (d: dt.type) = new AddODE(vs)
  }

  /** Object `dt` is used for writing ODEs with syntax: d(v1)/dt = ... */
  object dt

  /** The integrator object which can be any implementation compatible
    * with the Apache Commons Math ODE library. Free to override in
    * subclasses. By default we use the Dormand Prince 8,5,3 integrator
    * from their example documentation.
    */
  def integrator(): ode.FirstOrderIntegrator =
    new DormandPrince853Integrator(minStep, maxStep,
      absoluteTolerance, relativeTolerance)

  private val minStep = 1e-8
  private val maxStep = 100
  private val absoluteTolerance = 1e-10
  private val relativeTolerance = 1e-10
  private val simTime = Double("sim:t")

  /** Main function implementing the `Process` interface. */
  override def step(time: Double, tau: Double) {
//    println(s"Integrating ${time} -> ${time} + ${tau}")

    // construct array of doubles corresponding to the the values of
    // vars which is what the ODE solver will actually use
    val doubleY = vars.map(_.value).toArray
    assume(doubleY.size > 0, "we should have some ys to integrate")

    // construct the integrator
    val phi = integrator()

    // conduct the integration
    phi.integrate(this, time, doubleY, time+tau, doubleY)

    // put the results of the integration into the variables
    var i = 0
    while (i < doubleY.size) {
      vars(i) := doubleY(i)
      i += 1
    }

//    println("Integration complete")
  }

  /** This is the method that does the actual work. It must be implemented
    * by concrete sub-classes which should calculate the values of the
    * derivatives (third argument, `ydots`) in terms of the (given) first two.
    * Indexes into the arrays are as defined by `vars`.
    */
  def computeDerivatives(time: Double, ys: Array[Double], ydots: Array[Double]) {
    //print(s"computing Derivatives ${ys.toList} -> ")

    // set the time to the microscopic time-step time
    simTime := time

    var i = 0
    while (i < ydots.size) {
      // sanity check the input
      assume(funs isDefinedAt i, "no derivative defined for " + vars(i))
      assume(!ys(i).isNaN, "integration of " + vars(i).meta +
        " gave NaN (not a number)")

      // set the value of the variables so that the functions for
      // computing the derivatives can use them
      vars(i) := ys(i)
      // update the value of the ys(i) which will have had constraints
      // such as nonnegative() applied
      ys(i) = vars(i).value
      // now compute the derivatives
      ydots(i) = funs(i)()

      // and sanity check the output
      assume(!ydots(i).isNaN, "integration of " + vars(i).meta +
        " gave NaN (not a number)")
      i += 1
    }
    //println(s" ${ydots.toList}")
    //println(s"${simTime}\t${ys.toList}")
    //assume(false, "xxx")
  }

  /** This is required by the ODE solver and gives the dimension of
    * the vector-valued integral.
    */
  def getDimension = vars.size
}
