package uk.ac.ed.inf.mois

import language.experimental.macros
import reflect.macros.Context

import org.apache.commons.math3.ode.{FirstOrderIntegrator, FirstOrderDifferentialEquations}
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator

import collection.mutable

// TODO: Maybe we should allow users to define algebraic equations
// as well as we do in the graph-rewriting library.

/** A partial implementation of `Process` that uses the Apache Commons
  * Math ODE library to implement its `step` method. The `computeDerivatives`
  * method must be filled out to describe the system of differential equations
  */
abstract class ProcessODE(name: String) extends Process(name) with FirstOrderDifferentialEquations {

  /** A class to define derivatives of `Var`s. */
  class ODE(val v: NumericVar[Double]) {
    def := (e: Double): Unit = macro Macros.createFun
  }

  /** Adds an ODE definition to the process. */
  def addODE(v: NumericVar[Double], f: Derivative) = {
    indices += v -> (vars.size)
    vars += v
    funs += f
  }

  /** Object `dt` is used for writing ODEs with syntax: d(v1)/dt = ... */
  object dt

  /** `Var` used to construct derivatives that depend on time. */
  val t = Var(0.0, "time", Some("ProcessODE:" + name))

  /** Adds an ODE definition to the current `ProcessODE`. */
  def d(v: NumericVar[Double]) = new ODE(v) {
    def / (d: dt.type) = new ODE(v)
  }

  def eval(v: NumericVar[Double], ys: Array[Double]): Double =
    // if (indices contains v) ys(indices(v)) else v.value
    indices get v map ys getOrElse v.value

  /** A map that returns the index of a `Var` in `vars`. */
  val indices: mutable.Map[NumericVar[Double], Int] =
    mutable.Map.empty[NumericVar[Double], Int] withDefault (v =>
      throw new IllegalArgumentException("No differential equation " +
        "defined for Var(\"" + v.identifier + "\", \"" + v.scope +
        "\").  Define one using d(v) := ..."))

  /** An array with all `Var`s for which to integrate. */
  val vars: mutable.ArrayBuffer[NumericVar[Double]] =
    mutable.ArrayBuffer.empty[NumericVar[Double]]

  type Derivative = Array[Double] => Double

  /** Functions defining the derivatives of the variables in `vars`.
    * The two arrays are indexed equally.
    */
  val funs: mutable.ArrayBuffer[Derivative] =
    mutable.ArrayBuffer.empty[Derivative]

  /** The integrator object which can be any implementation compatible
    * with the Apache Commons Math ODE library. Free to override in
    * subclasses. By default we use the Dormand Prince 8,5,3 integrator
    * from their example documentation.
    */
  val integrator: () => FirstOrderIntegrator = () =>
    new DormandPrince853Integrator(1e-8, 100.0, 1e-10, 1e-10)

  /** Main function implementing the `Process` interface. */
  def step(time: Double, tau: Double) {
    // construct array of doubles corresponding to the the values of
    // vars which is what the ODE solver will actually use
    val doubleY = vars.map(_.value).toArray

    // set time
    t := time

    // conduct the integration
    integrator().integrate(this, time, doubleY, time+tau, doubleY)

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
    t := time
    for (i <- 0 until ydots.size)
      ydots(i) = funs(i)(ys)
  }

  /** This is required by the ODE solver and gives the dimension of
    * the vector-valued integral.
    */
  def getDimension = vars.size
}

object Macros {
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
    // transformer to replace Vars by a call to ProcessODE.eval
    object transformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case q"uk.ac.ed.inf.mois.Conversions.Var2Value[$t]($v)" =>
          q"eval($v, ys)"
        case _ => super.transform(tree)
      }
    }
    // construct function
    val fun = q"(ys => ${transformer.transform(e.tree)})"
    c.Expr[Unit](c.resetLocalAttrs(q"addODE($v, $fun)"))
  }
}
