package uk.ac.ed.inf.mois

import language.experimental.macros
import reflect.macros.Context

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
    extends ODEIntf with VarConversions {
  override def stringPrefix = "ODE"
}

abstract class ODEIntf extends BaseProcess
    with ode.FirstOrderDifferentialEquations {
  self =>

  /** A class to define derivatives of `Var`s. */
  protected class FunMaker(val v: DoubleVarIntf) {
    def := (e: Double): Unit = macro ODEMacros.createFun
  }

  /** Adds an ODE definition to the process. */
  def addODE(v: DoubleVarIntf, f: Derivative) = {
    indices += v -> (vars.size)
    vars += v
    funs += f
  }

  /** Adds an ODE definition to the current `ODE`. */
  protected def d(v: DoubleVarIntf) = new FunMaker(v) {
    def / (d: dt.type) = new FunMaker(v)
  }

  /** Object `dt` is used for writing ODEs with syntax: d(v1)/dt = ... */
  object dt

  /** `Var` used to construct derivatives that depend on time. */
  var t = 0.0

  @inline final def eval(v: DoubleVarIntf, ys: Array[Double]): Double =
    // if (indices contains v) ys(indices(v)) else v.value
    indices get v map ys getOrElse v.value

  /** A map that returns the index of a `Var` in `vars`. */
  val indices = mutable.Map.empty[DoubleVarIntf, Int] withDefault (v =>
    throw new IllegalArgumentException("No differential equation " +
      "defined for " + v + ".  Define one using d(v) := ..."))

  /** An array with all `Var`s for which to integrate. */
  val vars = mutable.ArrayBuffer.empty[DoubleVarIntf]

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
  def integrator(): ode.FirstOrderIntegrator =
    new DormandPrince853Integrator(minStep, maxStep,
      absoluteTolerance, relativeTolerance)

  // RHZ: for me to remember what these numbers are
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
      ydots(i) = funs(i)(ys)
    }
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
    // transformer that replaces Vars by a call to ODE.eval
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
