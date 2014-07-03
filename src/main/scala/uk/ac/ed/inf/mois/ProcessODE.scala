package uk.ac.ed.inf.mois

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

  // -- Monomials --

  // RHZ: I borrowed this code from the graph-rewriting library,
  // but we would probably like to use some computer algebra library
  // for this.  Or maybe this whole approach is just unnecessary,
  // what we actually need is just a way to transform something like
  // d(x1) := -0.3*x1 - 0.4*x2
  // (where `x1` and `x2` are `Var`s) into something like:
  // ys => -0.3 * ys(indices(x1)) - 0.4 * ys(indices(x2)))
  // where `ys` is the array passed to computeDerivates by the
  // integrator.  Afaik this is possible using macros, but is there
  // another option?  I'd prefer to avoid macros at this stage.
  // Even though this might seem utterly convoluted to only make
  // the syntax inside `ProcessODE` a bit nicer, I think syntax is
  // important for our potential users.

  /** A class for monomials.  These monomials are used to construct
    * polynomial functions (see `Polynomial`).
    */
  case class Monomial(coef: Double,
    factors : Vector[NumericVar[Double]],
    divisors: Vector[NumericVar[Double]]) {

    def * (n: Double) = Monomial(coef * n, factors, divisors)
    def / (n: Double) = Monomial(coef / n, factors, divisors)
    def * (g: NumericVar[Double]) = Monomial(coef, factors :+ g, divisors)
    def / (g: NumericVar[Double]) = Monomial(coef, factors, divisors :+ g)
    def * (m: Monomial) = Monomial(coef * m.coef,
      factors ++ m.factors, divisors ++ m.divisors)
    def / (m: Monomial) = Monomial(coef / m.coef,
      factors ++ m.divisors, divisors ++ m.factors)

    def apply(ys: Array[Double]): Double = coef *
      (for (v <- factors ) yield if (indices contains v) ys(indices(v)) else v.value).product /
      (for (v <- divisors) yield if (indices contains v) ys(indices(v)) else v.value).product

    override def toString = coef +
      (if (factors.isEmpty) ""
       else " " + factors.map("\"" + _.identifier + "\"").mkString(" ")) +
      (if (divisors.isEmpty) ""
       else " / (" + divisors.map("\"" + _.identifier + "\"").mkString(" ") + ")")
  }

  implicit def numToMonomial(n: Double) =
    Monomial(n, Vector(), Vector())
  implicit def varToMonomial(v: NumericVar[Double]) =
    Monomial(1, Vector(v), Vector())


  // -- Polynomials --

  /** A class for polynomial functions.  These polynomial functions
    * are used to define derivatives (see `d`).
    */
  class Polynomial(val terms: Vector[Monomial]) {
    def + (m: Monomial) = Polynomial(terms :+ m)
    def + (p: Polynomial) = Polynomial(terms ++ p.terms)
    def - (m: Monomial) = Polynomial(terms :+ (m * -1))
    def - (p: Polynomial) = Polynomial(terms ++ (p.terms match {
      case Vector() => Vector()
      case hd +: tl => (hd * -1) +: tl
    }))

    /** Apply this function to an array with the values for the
      * different `Var`s.  The order in which these variables
      * appear is the one given by `vars`, i.e. the order in which
      * the differential equations were defined using `d`.
      */
    def apply(ys: Array[Double]): Double =
      (for (m <- terms) yield m(ys)).sum

    override def toString =
      if (terms.isEmpty) "0"
      else terms.mkString(" + ").replace("+ -", "-")
  }

  object Polynomial {
    def apply() = new Polynomial(Vector())
    def apply(terms: Vector[Monomial]) = new Polynomial(terms)
    def apply(terms: Monomial*) = new Polynomial(terms.toVector)
  }

  implicit def numToPolynomial(n: Double) =
    Polynomial(Vector(numToMonomial(n)))
  implicit def varToPolynomial(v: NumericVar[Double]) =
    Polynomial(Vector(varToMonomial(v)))
  implicit def monomialToPolynomial(m: Monomial) =
    Polynomial(Vector(m))


  // -- ODEs --

  /** A class to define derivatives of `Var`s. */
  class ODE(v: NumericVar[Double]) {
    def := (p: Polynomial) {
      indices += v -> (vars.size)
      vars += v
      polys += p
    }
  }

  /** Object `dt` is used for writing ODEs with syntax: d(v1)/dt = ... */
  object dt

  /** `Var` used to construct derivatives that depend on time. */
  val t = Var(0.0, "time", Some("ProcessODE:" + name))

  // RHZ: Note that we could do something else here: we could define
  // a method `dt_=` and then define ODEs like: d(v1).dt = ...
  // Although the use of `:=` seems consistent with the way we set
  // values for `Var`s, so I'm more inclined to keep it as it is.
  /** Adds an ODE definition to the current `ProcessODE`. */
  def d(v: NumericVar[Double]) = new ODE(v) {
    def / (d: dt.type) = new ODE(v)
  }

  /** A map that returns the index of a `Var` in `vars`. */
  val indices: mutable.Map[NumericVar[Double], Int] =
    mutable.Map.empty[NumericVar[Double], Int] withDefault (v =>
      throw new IllegalArgumentException("No differential equation " +
        "defined for Var(\"" + v.identifier + "\", \"" + v.scope +
        "\").  Define one using d(v) := ..."))

  /** An array with all `Var`s for which to integrate. */
  val vars: mutable.ArrayBuffer[NumericVar[Double]] =
    mutable.ArrayBuffer.empty[NumericVar[Double]]

  /** Polynomial functions used to define the derivatives. */
  val polys: mutable.ArrayBuffer[Polynomial] =
    mutable.ArrayBuffer.empty[Polynomial]

  /** The integrator object which can be any implementation compatible
    * with the Apache Commons Math ODE library. Free to override in
    * subclasses. By default we use the Dormand Prince 8,5,3 integrator
    * from their example documentation.
    */
  val integrator: () => FirstOrderIntegrator = () =>
    new DormandPrince853Integrator(1e-8, 100.0, 1e-10, 1e-10)

  /** Main function implementing the `Process` interface. */
  def step(time: Double, tau: Double) {
    // construct array of doubles corresponding to the integral variables
    // which is what the ODE solver will actually use
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
      ydots(i) = polys(i)(ys)
  }

  /** This is required by the ODE solver and gives the dimension of
    * the vector-valued integral.
    */
  def getDimension = vars.size
}
