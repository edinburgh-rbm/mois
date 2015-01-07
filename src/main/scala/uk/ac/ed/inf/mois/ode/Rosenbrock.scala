/*
 *  MOIS: Ordinary Differential Equations with Apache Commons Math3
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
package uk.ac.ed.inf.mois.ode

import language.implicitConversions
import scala.math.{abs, sqrt, max, min}
import spire.math.{Jet, JetDim}
import spire.algebra.{Field, NRoot, Ring}
import spire.implicits._
import no.uib.cipr.{matrix => mtl}
import uk.ac.ed.inf.mois.{Process, Var}

trait Rosenbrock extends Process with ODEBase[Double, Jet[Double]] {
  implicit def jetDim = JetDim(vars.size)
  implicit def vToD(v: Var[Double]): Jet[Double] = {
    val idx = vars.indexOf(v)
    if (idx == -1) {
      v.value
    } else {
      v.value + Jet.h[Double](idx)
    }
  }
  implicit def toJetF(f: Var[Double]): () => Jet[Double] =
    (() => vToD(f))
  protected[mois] lazy val _rg = implicitly[Ring[Jet[Double]]]
  protected[mois] lazy val _nr = implicitly[NRoot[Jet[Double]]]
  protected[mois] lazy val _fd = implicitly[Field[Jet[Double]]]
  protected[mois] def _fromInt(i: Int): Jet[Double] = Jet.fromInt[Double](i)

  val t = Double("sim:t")
  vars += t
  funs += (() => 1.0)

  val atol = 1e-5
  val rtol = 1e-5

  def deriv: mtl.Matrix = {
    val dydt = new mtl.DenseMatrix(vars.size, 1)
    var i = 0
    while (i < vars.size) {
      val ydot = funs(i)()
      dydt.set(i, 0, ydot.real)
      i += 1
    }
    dydt
  }

  def jacobian: (mtl.Matrix, mtl.Matrix) = {
    val dydt = new mtl.DenseMatrix(vars.size, 1)
    val dfdy = new mtl.DenseMatrix(vars.size, vars.size)
    var i = 0
    while (i < vars.size) {
      val ydot = funs(i)()
      dydt.set(i, 0, ydot.real)
      var j = 0
      while (j < vars.size) {
        dfdy.set(i, j, ydot.infinitesimal(j))
        j += 1
      }
      i += 1
    }
    (dydt, dfdy)
  }

  def fromState: mtl.Matrix = {
    val y0 = new mtl.DenseMatrix(vars.size, 1)
    var i = 0
    while (i < vars.size) {
      y0.set(i, 0, vars(i).value)
      i += 1
    }
    y0
  }

  def toState(y0: mtl.Matrix) {
    var i = 0
    while (i < vars.size) {
      vars(i) := y0.get(i, 0)
      i += 1
    }
  }

  // we remember the optimal step size for where we are so
  // that we don't have to figure it out with each macroscopic
  // time step
  private var hopt = 0.0
  override def step(t0: Double, tau: Double) {
//    println(s"Rosenbrock: ${vars}")
    val y0 = fromState

    var y    = y0
    var t    = t0
    var h    = if (hopt == 0.0) tau else min(tau, hopt)
    var done = false

    while(!done) {
      // try a rosenbrock step
      val (ynext, err) = rosStep(y, t, h)
      // see if the truncation error is acceptable
      val (hnext, rejected) = success(err, h)
//      println(s"step size: ${h} err: ${err}")

      if (!rejected) { // all good, move forward
        y = ynext
        t = t + h
        if (t >= t0 + tau) { // finished! save and return
          toState(y)
          done = true
        }
      }
      hopt = hnext // save what we figured out was the optimal 
                   // step size
      // don't go past the end of our requested interval
      h = min(hnext, t0 + tau - t)
    }
//    println(s"done: ${t0} + ${tau} ~ ${t}")
  }

  // the adaptive step-size picker uses some information from
  // previous time steps, so these are kept here. this is pretty much 
  // straight out of chapter 17 of NR
  private var hold = 0.0
  private var errold = 0.0
  private var first_step = true
  private var reject = false
  def success(err: Double, h: Double): (Double, Boolean) = {
    val safe = 0.9
    val fac1 = 5.0
    val fac2 = 1.0/6.0
    var fac = max(fac2, min(fac1, err.pow(0.25)/safe))
    var hnew = h / fac
    if (err <= 1.0) {// success
      if (!first_step) { // predictive control
        val tmp = (hold/h) * (err*err/errold).pow(0.25) / safe
        val facpred = max(fac2, min(fac1, tmp))
        fac = max(fac, facpred)
        hnew = h / fac
      }
      first_step = false
      hold = h
      errold = max(0.01, err)
      if (reject) // don't let step increase if last one was rejected
        hnew = if (h > 0.0) min(hnew, h) else max(hnew, h)
      reject = false
    } else { // Truncation error too large, reduce stepsize
//      println(s"reject")
      reject = true
    }
    (hnew, reject)
  }

  // The rosenbrock step itself...
  def rosStep(y0: mtl.Matrix, t0: Double, h: Double): (mtl.Matrix, Double) = {
    import rosenbrockConst._

    toState(y0)
    t := t0
    val (dydt, dfdy) = jacobian

    /* organise (1/\gamma h - f') */
    val igh   = mtl.Matrices.identity(vars.size).scale(1.0 / (gam * h))
    val a     = igh.add(dfdy.scale(-1)).asInstanceOf[mtl.DenseMatrix]

    /* LU decomposition */
    val lu    = new mtl.DenseLU(vars.size, vars.size)
    lu.factor(a)

    /* solve for g1 */
    val g1    = dydt.copy
    lu.solve(g1.asInstanceOf[mtl.DenseMatrix])
//    println(g1)

    /* solve for g2 */
    toState(g1.copy.scale(a21).add(y0))
    t := t0 + c2*h
    val g2 = deriv
    g2.add(g1.copy.scale(c21/h))
    lu.solve(g2.asInstanceOf[mtl.DenseMatrix])
//    println(g2)

    /* solve for g3 */
    toState(g1.copy.scale(a31).add(g2.copy.scale(a32)).add(y0))
    t := t0 + c3*h
    val g3 = deriv
    g3.add(g1.copy.scale(c31/h).add(g2.copy.scale(c32/h)))
    lu.solve(g3.asInstanceOf[mtl.DenseMatrix])
//    println(g3)

    /* solve for g4 */
    toState(
      g1.copy.scale(a41)
        .add(g2.copy.scale(a42))
        .add(g3.copy.scale(a43))
        .add(y0)
    )
    t := t0 + c4*h
    val g4 = deriv
    g4.add(
      g1.copy.scale(c41/h)
        .add(g2.copy.scale(c42/h))
        .add(g3.copy.scale(c43/h))
    )
    lu.solve(g4.asInstanceOf[mtl.DenseMatrix])
//    println(g4)

    /* solve for g5 */
    val tmp = g1.copy.scale(a51)
        .add(g2.copy.scale(a52))
        .add(g3.copy.scale(a53))
        .add(g4.copy.scale(a54))
        .add(y0)
    toState(tmp)
    t := t0 + h
    val g5 = deriv
    g5.add(
      g1.copy.scale(c51/h)
        .add(g2.copy.scale(c52/h))
        .add(g3.copy.scale(c53/h))
        .add(g4.copy.scale(c54/h))
    )
    lu.solve(g5.asInstanceOf[mtl.DenseMatrix])
//    println(g5)

    tmp.add(g5)
    toState(tmp)
    t := t0 + h
    val g6 = deriv
    g6.add(
      g1.copy.scale(c61/h)
        .add(g2.copy.scale(c62/h))
        .add(g3.copy.scale(c63/h))
        .add(g4.copy.scale(c64/h))
        .add(g5.copy.scale(c65/h)))
    lu.solve(g6.asInstanceOf[mtl.DenseMatrix])
    val err = g6
    tmp.add(g6)

//    println(tmp)

    (tmp, error(y0, tmp, err))
  }

  // calculate error
  def error(y0: mtl.Matrix, y: mtl.Matrix, yerr: mtl.Matrix): Double = {
    var err: Double = 0.0
    var i = 0
    while (i < vars.size) {
      val sk = atol + rtol * max(abs(y0.get(i, 0)), abs(y.get(i, 0)))
      err += (yerr.get(i, 0)/sk).pow(2)
      i += 1
    }
    sqrt(err/vars.size)
  }
}

object rosenbrockConst {
  val c2    : Double =  0.386
  val c3    : Double =  0.21
  val c4    : Double =  0.63
  val bet2p : Double =  0.0317
  val bet3p : Double =  0.0635
  val bet4p : Double =  0.3438
  val d1    : Double =  0.2500000000000000e+00
  val d2    : Double = -0.1043000000000000e+00
  val d3    : Double =  0.1035000000000000e+00
  val d4    : Double = -0.3620000000000023e-01
  val a21   : Double =  0.1544000000000000e+01
  val a31   : Double =  0.9466785280815826e+00
  val a32   : Double =  0.2557011698983284e+00
  val a41   : Double =  0.3314825187068521e+01
  val a42   : Double =  0.2896124015972201e+01
  val a43   : Double =  0.9986419139977817e+00
  val a51   : Double =  0.1221224509226641e+01
  val a52   : Double =  0.6019134481288629e+01
  val a53   : Double =  0.1253708332932087e+02
  val a54   : Double = -0.6878860361058950e+00
  val c21   : Double = -0.5668800000000000e+01
  val c31   : Double = -0.2430093356833875e+01
  val c32   : Double = -0.2063599157091915e+00
  val c41   : Double = -0.1073529058151375e+00
  val c42   : Double = -0.9594562251023355e+01
  val c43   : Double = -0.2047028614809616e+02
  val c51   : Double =  0.7496443313967647e+01
  val c52   : Double = -0.1024680431464352e+02
  val c53   : Double = -0.3399990352819905e+02
  val c54   : Double =  0.1170890893206160e+02
  val c61   : Double =  0.8083246795921522e+01
  val c62   : Double = -0.7981132988064893e+01
  val c63   : Double = -0.3152159432874371e+02
  val c64   : Double =  0.1631930543123136e+02
  val c65   : Double = -0.6058818238834054e+01
  val gam   : Double =  0.2500000000000000e+00
  val d21   : Double =  0.1012623508344586e+02
  val d22   : Double = -0.7487995877610167e+01
  val d23   : Double = -0.3480091861555747e+02
  val d24   : Double = -0.7992771707568823e+01
  val d25   : Double =  0.1025137723295662e+01
  val d31   : Double = -0.6762803392801253e+00
  val d32   : Double =  0.6087714651680015e+01
  val d33   : Double =  0.1643084320892478e+02
  val d34   : Double =  0.2476722511418386e+02
  val d35   : Double = -0.6594389125716872e+01
}










