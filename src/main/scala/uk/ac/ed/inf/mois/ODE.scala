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

// TODO: Maybe we should allow users to define algebraic equations
// as well as we do in the graph-rewriting library.

/** A partial implementation of `Process` that uses the Apache Commons
  * Math ODE library to implement its `step` method.
  */
abstract class ODE(val name: String)
    extends BaseODE with VarConversions {
  override def stringPrefix = "ODE"
}

abstract class BaseODE
    extends BaseProcess
       with ode.FirstOrderDifferentialEquations {
  self =>

  /** A class to define derivatives of `Var`s. */
  protected class AddODE(val vs: Seq[DoubleVarIntf]) {

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
  implicit def varToFun(f: DoubleVarIntf) = () => f.value

  /** Adds an ODE definition to the current `ODE`. */
  protected def d(vs: DoubleVarIntf*) = new AddODE(vs) {
    def / (d: dt.type) = new AddODE(vs)
  }

  /** Object `dt` is used for writing ODEs with syntax: d(v1)/dt = ... */
  object dt

  /** `Var` used to construct derivatives that depend on time. */
  var t = 0.0

  /** An array with all `Var`s for which to integrate. */
  val vars = mutable.ArrayBuffer.empty[DoubleVarIntf]

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
  def step(time: Double, tau: Double) {
    // construct array of doubles corresponding to the the values of
    // vars which is what the ODE solver will actually use
    val doubleY = vars.map(_.value).toArray

    // set time
    t = time

    // construct the integrator
    val i = integrator()

    // only add step handlers if we have them
    //
    // WW: this block is ***required*** do not remove
    //
    // RHZ: No! This is not required! An explanation of why you think
    // it's required would helpful, because after last time we
    // discussed it I had the impression we agreed it was not even
    // desired.
    //
    // The reason I think it's *wrong* to do this is that mois should
    // treat all processes as black boxes.  Also, mois step handlers
    // in my opinion should have the following guarantee: they will
    // be called only once per call to step.  They should handle mois
    // steps, not someone's else steps.  This clearly violates both.
    //
    // WW: because it is required to be able to produce a trace of
    // output for an ODE process. There might be another way to do this
    // by manipulating the way the process is run but until that exists
    // and we are satisfied with it, do not remove the ability to
    // because I need it and I use it. It's impossible to, e.g.
    // work on schedulers and compare to a known behaviour without it.
    //
    // We agreed that it was a bit ugly and should go away but not
    // without an alternative
    if (stepHandlers.size > 0) {
      i.addStepHandler(new sampling.StepHandler {
        def init(t0: Double, y0: Array[Double], t: Double) {}
        def handleStep(interp: sampling.StepInterpolator, isLast: Boolean) {
          val t = interp.getCurrentTime()
          val y = interp.getInterpolatedState()
          for (i <- 0 until vars.size)
            vars(i) := y(i)
          for (sh <- stepHandlers)
            sh.handleStep(t, self)
        }
      })
    }

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
      vars(i) := ys(i)
      ydots(i) = funs(i)()
    }
  }

  /** This is required by the ODE solver and gives the dimension of
    * the vector-valued integral.
    */
  def getDimension = vars.size

  @inline override def apply(t: Double, tau: Double) = step(t, tau)
}
