/*
 *  MOIS: Variables Test
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

import uk.ac.ed.inf.mois.{ArrayBackedStateBuilder, ConstraintViolation}

class VarTest extends FlatSpec with Matchers with ArrayBackedStateBuilder {
  // Use approximate equality in `should equal` for doubles
  val precision = 1e-8
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  val i1 = Int("i1")
  val i2 = Int("i2")
  val b1 = Boolean("b1")
  val b2 = Boolean("b2")
  val d1 = Double("d1")
  val d2 = Double("d2")
  val xd2 = Double("d2") // alias
  val state = buildState
  initState(state)

  "state variables" should "support arithmetic operations" in {
    import uk.ac.ed.inf.mois.implicits._
    i1 := 1
    i2 := 2
    (i1 + i2) should be (3)
  }

  it should "not care too much about types for arithmetic" in {
    import spire.implicits._
    import uk.ac.ed.inf.mois.implicits._
    d1 := 0
    d1 += 1
    (d1.value) should be (1.0)
  }

  it should "support difference" in {
    import uk.ac.ed.inf.mois.implicits._
    d1 := 0.5
    d2 := 0.8
    (d2 - d1) should equal (0.3)
  }

  it should "have unambiguous keys" in {
    d2.meta should be (xd2.meta)
    d2.meta should not be (d1.meta)

    d2 := 5.0
    xd2.value should equal (5.0)
    xd2 := 0.0
    d2.value should equal (0.0)
  }

  it should "have natural syntax for numerical operations" in {
    import spire.implicits._
    import uk.ac.ed.inf.mois.implicits._
    i1 := 2
    i1.value should be (2)

    i2 := 2 + 2*i1
    i2.value should be (6)

    val d = i2 - i1
    (2 * d) should be (8)

    i1 += d
    i1.value should be (i2.value)

    (i1 - d) should be (i2 - d)
  }
}
