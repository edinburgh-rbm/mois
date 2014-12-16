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

import scala.language.reflectiveCalls
import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.Math
import uk.ac.ed.inf.mois.ode.ODE
import uk.ac.ed.inf.mois.implicits._

/** Directly calculated ODE system from Dominik's stuff. */
object sampleODE extends ODE with Math {
  val t  = Double("sim:t")
  val x1 = Double("ex:x1")
  val x2 = Double("ex:x2")
  val x3 = Double("ex:x3")
  val x4 = Double("ex:x4")
  d(x1)/dt := -0.3*x1 - 0.4*x2
  d(x2)/dt := -0.5*x1 - 0.8*x2
  d(x3)/dt := sin(t)
  d(x4)/dt := x1

  init(0)
}

/** Directly calculated ODE system from Dominik's stuff. */
object sampleODE2 extends ODE with Math {
  val t  = Double("sim:t")
  val x1 = Double("ex:x1")
  val x2 = Double("ex:x2")
  val x3 = Double("ex:x3")
  val x4 = Double("ex:x4")
  d(x1, x2, x3, x4)/dt := (
    -0.3*x1 - 0.4*x2,
    -0.5*x1 - 0.8*x2,
    sin(t),
    x1
  )

  init(0)
}

class ODETest extends FlatSpec with Matchers {

  // Use approximate equality in `should equal`
  val precision = 1e-4
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "sample ODE" should "give Dominik's expected results" in {

    sampleODE.x1 := 25.0
    sampleODE.x2 := 50.0
    sampleODE.x3 := 0.0

    sampleODE.x1.value should equal (25.0)
    sampleODE.x2.value should equal (50.0)
    sampleODE.x3.value should equal (0.0)

    // Integrate from t1 = 0 to t2 = 50
    sampleODE.step(0, 50)

    sampleODE.x1.value should equal (-0.1398)
    sampleODE.x2.value should equal (0.0916)
    sampleODE.x3.value should equal (0.0350)

    // Integrate from t1 = 50 to t2 = 150
    sampleODE.step(50, 100)

    sampleODE.x1.value should equal (-0.0032)
    sampleODE.x2.value should equal (0.0021)
    sampleODE.x3.value should equal (0.3007)

    // reset the initial conditions
    sampleODE.x1 := 25.0
    sampleODE.x2 := 50.0
    sampleODE.x3 := 0.0

    // make sure we get the same results
    sampleODE.step(0, 50.0)
    sampleODE.x1.value should equal (-0.1398)
    sampleODE.x2.value should equal (0.0916)
    sampleODE.x3.value should equal (0.0350)
  }

  it should "also give the results with fancy syntax" in {
    sampleODE2.x1 := 25.0
    sampleODE2.x2 := 50.0
    sampleODE2.x3 := 0.0

    sampleODE2.x1.value should equal (25.0)
    sampleODE2.x2.value should equal (50.0)
    sampleODE2.x3.value should equal (0.0)

    // Integrate from t1 = 0 to t2 = 50
    sampleODE2.step(0, 50)

    sampleODE2.x1.value should equal (-0.1398)
    sampleODE2.x2.value should equal (0.0916)
    sampleODE2.x3.value should equal (0.0350)

    // Integrate from t1 = 50 to t2 = 150
    sampleODE2.step(50, 100)

    sampleODE2.x1.value should equal (-0.0032)
    sampleODE2.x2.value should equal (0.0021)
    sampleODE2.x3.value should equal (0.3007)

    // reset the initial conditions
    sampleODE2.x1 := 25.0
    sampleODE2.x2 := 50.0
    sampleODE2.x3 := 0.0

    // make sure we get the same results
    sampleODE2.step(0, 50.0)
    sampleODE2.x1.value should equal (-0.1398)
    sampleODE2.x2.value should equal (0.0916)
    sampleODE2.x3.value should equal (0.0350)
  }

  it should "error with fancy syntax incorrectly applied" in {
    intercept[IllegalArgumentException] {
      class Wrong extends ODE {
        val x1 = Double("x1")
        val x2 = Double("x2")

        d(x1) := (x1, x2)
      }
      val wrong = new Wrong
    }
  }
}

class PartialDerivativeTest extends FlatSpec with Matchers {

  // Use approximate equality in `should equal`
  val precision = 1e-4
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  class P extends ODE {
    val x1 = Double("x1")
    val x2 = Double("x2")

    d(x1, x2) := (
      x1,
      x1
    )
  }

//  "partial derivative" should "give expected results" in {
    // val p = new P
    // val partials = p partialDerivatives(0, 1)

    // val dp_dx1 = partials(p.x1)
    // dp_dx1(p.x1).value should equal (0.0)
    // dp_dx1(p.x2).value should equal (1.7182)

    // val dp_dx2 = partials(p.x2)
    // dp_dx2(p.x1).value should equal (0.0)
    // dp_dx2(p.x2).value should equal (0.0)
//  }
}
