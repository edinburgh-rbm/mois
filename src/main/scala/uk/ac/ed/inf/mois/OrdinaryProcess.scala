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

import language.experimental.macros
import reflect.macros.Context

import org.apache.commons.math3.ode
import org.apache.commons.math3.ode.sampling
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator

import collection.mutable

// TODO: Maybe we should allow users to define algebraic equations
// as well as we do in the graph-rewriting library.

// RHZ: OrdinaryProcess sounds to me as CommonProcess, isn't that what
// ordinary means in english?
/** A partial implementation of `Process` that uses the Apache Commons
  * Math ODE library to implement its `step` method. The `computeDerivatives`
  * method must be filled out to describe the system of differential equations
  */
abstract class OrdinaryProcess(name: String)
    extends Process(name)
       with ode.FirstOrderDifferentialEquations {

  /** A class to define derivatives of `Var`s. */
  class ODE(val v: DoubleVar) {
    def := (e: Double): Unit = macro ODEMacros.createFun
  }

  /** Adds an ODE definition to the process. */
  def addODE(v: DoubleVar, f: Derivative) = {
    indices += v -> (vars.size)
    vars += v
    funs += f
  }

  /** Object `dt` is used for writing ODEs with syntax: d(v1)/dt = ... */
  object dt

  /** `Var` used to construct derivatives that depend on time. */
  var t = 0.0

  /** Adds an ODE definition to the current `OrdinaryProcess`. */
  def d(v: DoubleVar) = new ODE(v) {
    def / (d: dt.type) = new ODE(v)
  }

  @inline final def eval(v: DoubleVar, ys: Array[Double]): Double =
    // if (indices contains v) ys(indices(v)) else v.value
    indices get v map ys getOrElse v.value

  /** A map that returns the index of a `Var` in `vars`. */
  val indices = mutable.Map.empty[DoubleVar, Int] withDefault (v =>
    throw new IllegalArgumentException("No differential equation " +
      "defined for " + v + ".  Define one using d(v) := ..."))

  /** An array with all `Var`s for which to integrate. */
  val vars = mutable.ArrayBuffer.empty[DoubleVar]

  type Derivative = Array[Double] => Double

  /** Functions defining the derivatives of the variables in `vars`.
    * The two arrays are indexed equally.
    */
  val funs = mutable.ArrayBuffer.empty[Derivative]

  /** The integrator object which can be any implementation compatible
    * with the Apache Commons Math ODE library. Free to override in
    * subclasses. By default we use the Dormand Prince 8,5,3 integrator
    * from their example documentation.
    */
  val integrator: () => ode.FirstOrderIntegrator = () =>
    new DormandPrince853Integrator(1e-8, 100.0, 1e-10, 1e-10)

  /** Main function implementing the `Process` interface. */
  def step(time: Double, tau: Double) {
    // construct array of doubles corresponding to the the values of
    // vars which is what the ODE solver will actually use
    val doubleY = vars.map(_.value).toArray

    // set time
    t = time

    // construct the integrator
    val i = integrator()

    // RHZ: why renaming this to proc for just one use?
    val proc = this
    // only add step handlers if we have them
    if (stepHandlers.size > 0) {
      object SH extends sampling.StepHandler {
        def init(t0: Double, y0: Array[Double], t: Double) {}
        def handleStep(interp: sampling.StepInterpolator, isLast: Boolean) {
          val t = interp.getCurrentTime()
          val ydot = interp.getInterpolatedState()
          for (i <- 0 until vars.size)
            vars(i) := ydot(i)
          for (sh <- stepHandlers)
            sh.handleStep(t, proc)
        }
      }
      i.addStepHandler(SH)
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
    for (i <- 0 until ydots.size)
      ydots(i) = funs(i)(ys)
  }

  /** This is required by the ODE solver and gives the dimension of
    * the vector-valued integral.
    */
  def getDimension = vars.size

  @inline override def apply(t: Double, tau: Double) = step(t, tau)
}

object ODEMacros {
  def createFun(c: Context)(e: c.Expr[Double]): c.Expr[Unit] = {
    import c.universe._
    // v is the variable for which we are defining the ODE
    // this is just to make the generated code nicer, it could be
    // just val v = q"${c.prefix.tree}.v" as well
    val v = c.prefix.tree match {
      case q"$x.this.d($v)" => v
      case q"$x.this.d($v)./($y.this.dt)" => v
      case _ => q"${c.prefix.tree}.v"
    }
    // transformer to replace Vars by a call to OrdinaryProcess.eval
    object transformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case q"$p.this.getVarValue[$t]($v)" => q"eval($v, ys)"
        case _ => super.transform(tree)
      }
    }
    // construct function
    val fun = q"(ys => ${transformer.transform(e.tree)})"
    c.Expr[Unit](c.resetLocalAttrs(q"addODE($v, $fun)"))
  }
}
