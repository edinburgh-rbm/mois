/*
 *  MOIS: VarCalc Process Helper Trait Test
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

import uk.ac.ed.inf.mois.{Process, VarCalc}
import uk.ac.ed.inf.mois.implicits._

class VarCalcTest extends FlatSpec with Matchers {
  class V extends Process with VarCalc {
    val x = Double("x")
    val y = Double("y")
    calc(y) := x
    override def step(t: Double, tau: Double) {
      x := t + tau
    }
  }

  "process with VarCalc" should "do assignments after steps" in {
    // Use approximate equality in `should equal`
    val precision = 1e-4
    implicit val doubleEquality =
      TolerantNumerics.tolerantDoubleEquality(precision)

    val vs = new V
    vs.init(0)

    import vs._

    x := 0.0
    y := 0.0

    vs(0, 1)

    x.value should equal (1.0)
    y.value should equal (x.value)

    vs(1, 4)

    x.value should equal (5.0)
    y.value should equal (x.value)
  }
}
