/*
 *  MOIS: Discrete-time Process Test
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
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._

import uk.ac.ed.inf.mois.DiscreteProcess

class Henon(a: Double, b: Double) extends DiscreteProcess[Double] {
  val x = Double("ex:x")
  val y = Double("ex:y")
  next(x) := 1.0 - a * x*x + y
  next(y) := b * x
}


class DiscreteProcessTest extends FlatSpec with Matchers {
  "henon process" should "give correct results" in {
    // Use approximate equality in `should equal`
    val precision = 1e-4
    implicit val doubleEquality =
      TolerantNumerics.tolerantDoubleEquality(precision)

    val henon = new Henon(1.4, 0.3)
    henon.init(0)

    import henon._

    henon.x := 0.0
    henon.y := 0.0

    henon.step(0, 1)

    henon.x.value should equal (1.0)
    henon.y.value should equal (0.0)

    henon.step(0, 5)

    henon.x.value should equal (0.3475)
    henon.y.value should equal (0.1663)
  }
}
