package uk.ac.ed.inf.mois.math

/**
  * y_{n+1} = y_n + hf(y_{n+1}, t_{n+1})
  *         \approx y_n + h(f(y_n) + D_y(f)(y_n)(y_{n+1}-y_n) + D_t(f)(t_{n+1}-t_n))
  *
  * (1 - hD_y(f)(y_n) - h^2Dt(f)(t_n))(y_{n+1} - y_n) = hf(y_n)
  */

import org.apache.commons.math3.ode
import no.uib.cipr.matrix
import scala.math.abs

class ReverseEulerIntegrator(stepSize: Double) extends ode.AbstractIntegrator {
  def updateJacobian(equations: ode.ExpandableStatefulODE, jacobian: matrix.Matrix) {
    val t = equations.getTime
    val y = equations.getCompleteState.clone
    val yDotL = Array.fill(y.size)(0.0)
    val yDotR = Array.fill(y.size)(0.0)

    equations.computeDerivatives(t, y, yDotL)

    for (i <- 0 until y.size) {
      val yi = y(i)
      val dyi = yDotL(i)*stepSize
      y(i) = yi + dyi
      equations.computeDerivatives(t, y, yDotR)
//      println(s"right ${yDotR.toList}")

      for (j <- 0 until y.size) {
        val dfjdyi = (yDotR(j) - yDotL(j))/dyi
//        println(s"df${j}/dy${i} = ${dfjdyi}")
        jacobian.set(i, j, dfjdyi)
      }

      y(i) = yi
    }
  }

  def updateTimeDerivative(equations: ode.ExpandableStatefulODE, dydt: matrix.DenseVector) {
    val t = equations.getTime
    val y = equations.getCompleteState
    val yDotL = Array.fill(y.size)(0.0)
    val yDotR = Array.fill(y.size)(0.0)

    equations.computeDerivatives(t-stepSize/2, y, yDotL)
    equations.computeDerivatives(t+stepSize/2, y, yDotR)
    for (i <- 0 until y.size) {
      dydt.set(i, (yDotR(i) - yDotL(i))/stepSize)
    }
  }

  def integrate(equations: ode.ExpandableStatefulODE, t: Double) {
//    println("XXX")

    val t0 = equations.getTime
    var stepStart = t0

    val y = new matrix.DenseVector(equations.getCompleteState)
    val dy = new matrix.DenseVector(y.size)

    val yDot = Array.fill(y.size)(0.0)
    val dydt = new matrix.DenseVector(y.size) // partials...
    val jacobian = new matrix.DenseMatrix(y.size, y.size)
    val identity = new matrix.DenseMatrix(y.size, y.size)
    for (i <- 0 until y.size)
      identity.set(i, i, 1.0)

    // val interpolator =
    initIntegration(equations.getTime, equations.getCompleteState, t)

    while(stepStart < t) {
      updateTimeDerivative(equations, dydt)
      dydt.scale(stepSize*stepSize)

      equations.computeDerivatives(stepStart, y.getData, yDot)
      val rhs = new matrix.DenseVector(yDot).scale(stepSize).add(dydt)

      updateJacobian(equations, jacobian)
//      println(s"${jacobian}")
      val M = jacobian.multAdd(-1, identity, identity.copy)

//      println(s"${M}")
      M.solve(rhs, dy)

      val check = M.mult(dy, dy.copy)

//      println(s"${dy}")
      y.add(dy)
//      println(s"${y}")
      stepStart += stepSize

//      println(s"${stepStart} ${y.get(0)} ${y.get(1)}")

//      interpolator.setTime(stepStart + stepSize)
//      stepStart = acceptStep(interpolator, y, yDot, t)
    }

    equations.setTime(stepStart)
    equations.setCompleteState(y.getData)
  }
}
