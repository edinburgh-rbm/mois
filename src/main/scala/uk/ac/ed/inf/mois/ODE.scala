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

/** A partial implementation of `Process` that uses the Apache Commons
  * Math ODE library to implement its `step` method.
  */
abstract class ODE extends Process
    with ode.FirstOrderDifferentialEquations {
  self =>

  /** An array with all `Var`s for which to integrate. */
  val vars = mutable.ArrayBuffer.empty[Index[Double]]

  /** A class to define derivatives of `Var`s. */
  protected class AddODE(val vs: Seq[Index[Double]]) {

    /** Adds an ODE definition to the process. */
    def := (fs: (() => Double)*): Unit = {
      require(fs.size == vs.size,
        "lhs and rhs of ODE system must have same size")
      for ((v, f) <- vs zip fs) {
        vars += v
        funs += f
      }
    }
  }
  implicit def bynameToFun(f: => Double) = () => f
  implicit def varToFun(f: Index[Double]) = () => f.value

  /** Adds an ODE definition to the current `ODE`. */
  protected def d(vs: Index[Double]*) = new AddODE(vs) {
    def / (d: dt.type) = new AddODE(vs)
  }

  /** Object `dt` is used for writing ODEs with syntax: d(v1)/dt = ... */
  object dt

  /** `Var` used to construct derivatives that depend on time. */
  var t = 0.0

  type Derivative = () => Double

  /** Functions defining the derivatives of the variables in `vars`.
    * The two arrays are indexed equally.
    */
  val funs = mutable.ArrayBuffer.empty[Derivative]

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

  /** Main function implementing the `Process` interface. */
  override def step(time: Double, tau: Double) {
    // construct array of doubles corresponding to the the values of
    // vars which is what the ODE solver will actually use
    val doubleY = vars.map(_.value).toArray

    // set time
    t = time

    // construct the integrator
    val i = integrator()

    // conduct the integration
    i.integrate(this, time, doubleY, time+tau, doubleY)

    // put the results of the integration into the variables
    for (i <- 0 until vars.size)
      vars(i) := doubleY(i)
  }

  /** This is the method that does the actual work. It must be implemented
    * by concrete sub-classes which should calculate the values of the
    * derivatives (third argument, `ydots`) in terms of the (given) first two.
    * Indexes into the arrays are as defined by `vars`.
    */
  def computeDerivatives(time: Double, ys: Array[Double], ydots: Array[Double]) {
    t = time
    for (i <- 0 until ydots.size) {
      assume(funs isDefinedAt i, "no derivative defined for " + vars(i))
      assume(!ys(i).isNaN, "integration of " + vars(i).meta +
        " gave NaN (not a number)")
      vars(i) := ys(i)
      ydots(i) = funs(i)()
    }
  }

  /** This is required by the ODE solver and gives the dimension of
    * the vector-valued integral.
    */
  def getDimension = vars.size
}
