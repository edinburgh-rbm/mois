/*
 *  MOIS: Ordinary Differential Equation Test
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
package uk.ac.ed.inf.mois.test

// import org.scalatest.{FlatSpec, Matchers}
// import org.scalactic.TolerantNumerics

// import org.apache.commons.math3.ode
// import uk.ac.ed.inf.mois.math.ReverseEulerIntegrator

// class ReverseEulerTest extends FlatSpec with Matchers {

//   // Use approximate equality in `should equal`
//   val precision = 1e-4
//   implicit val doubleEquality =
//     TolerantNumerics.tolerantDoubleEquality(precision)

//   private val nSteps = 4
//   private val minStep = 1e-8
//   private val maxStep = 100
//   private val absoluteTolerance = 1e-10
//   private val relativeTolerance = 1e-10

//   object predPrey extends ode.FirstOrderDifferentialEquations {
//     val alpha = 1.3
//     val beta = 0.5
//     val gamma = 1.6
//     val delta = 0.1
//     def getDimension = 2
//     def computeDerivatives(time: Double, y: Array[Double], ydot: Array[Double]) {
//       ydot(0) = y(0) * (alpha - beta * y(1))
//       ydot(1) = -y(1) * (gamma - delta * y(0))
//     }
//   }

//   /*
//    df0/dy0 = alpha - beta y1 = 1.3 - 0.5 * 3 = -0.19
//    df0/dy1 = -beta * y0 = -0.5 * 1 = -0.5

//    df1/dy0 = y1*delta = 3 * 0.1 = 0.3
//    df1/dy1 = -gamma + delta *y0 = -1.6 + 0.1 = -1.5


//    forward euler, 1 time step,
//    y0 = [1.0, 3.0]
//    df0/dt = 1.0 * ( 1.3 - 0.5 * 3) = -0.19
//    df1/dt = -3.0 * ( 1.6 - 0.1 * 1.0) = -4.5
//    y_1 = [1.0, 3.0] + [-0.19, -4.5]*0.01 = [ 0.9981, 2.955 ]

//    reverse euler,

//    [ 1 0 ] - [ -0.2  0.3 ] = [ 1.2 -0.3 ]
//    [ 0 1 ]   [ -0.5 -1.5 ]   [ 0.5  2.5 ]

//    [ 1.2 0.3 ] dy = [-0.0019]
//    [ 0.5 2.5 ]      [-0.0450]
//    */
//   val y0 = Array(1.0, 3.0)

//   "ReverseEulerIntegrator" should "give some answers" in {

//     val dpi = new ode.nonstiff.DormandPrince853Integrator(minStep, maxStep,
//       absoluteTolerance, relativeTolerance)
//     val dpiResult = Array.fill(y0.size)(0.0)
//     dpi.integrate(predPrey, 0, y0, 1, dpiResult)
//     println(dpiResult.toSeq)

//     val ami = new ode.nonstiff.AdamsMoultonIntegrator(nSteps, minStep, maxStep, 1e-5, 1e-5)
//     val amiResult = Array.fill(y0.size)(0.0)
//     ami.integrate(predPrey, 0, y0, 1, amiResult)
//     println(amiResult.toSeq)

//     val fei = new ode.nonstiff.EulerIntegrator(0.01)
//     val feiResult = Array.fill(y0.size)(0.0)
//     fei.integrate(predPrey, 0, y0, 1, feiResult)
//     println(feiResult.toSeq)

//     val rei = new ReverseEulerIntegrator(0.01)
//     val reiResult = Array.fill(y0.size)(0.0)
//     rei.integrate(predPrey, 0, y0, 1, reiResult)
//     println(reiResult.toSeq)

//     (true) should be (true)
//   }
// }
