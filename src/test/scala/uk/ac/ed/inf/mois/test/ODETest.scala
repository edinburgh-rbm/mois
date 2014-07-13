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

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.ODE

/** Directly calculated ODE system from Dominik's stuff. */
object sampleODE extends ODE("sample") {
  val x1 = Double("ex:x1")
  val x2 = Double("ex:x2")
  val x3 = Double("ex:x3")
  d(x1)/dt := -0.3*x1 - 0.4*x2
  d(x2)/dt := -0.5*x1 - 0.8*x2
  d(x3)/dt := java.lang.Math.sin(t)
}

class ODETest extends FlatSpec with Matchers {

  "sample ODE" should "give Dominik's expected results" in {

    // Use approximate equality in `should equal`
    val precision = 1e-4
    implicit val doubleEquality =
      TolerantNumerics.tolerantDoubleEquality(precision)

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
}