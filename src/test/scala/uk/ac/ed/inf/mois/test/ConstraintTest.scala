/*
 *  MOIS: Constraints Test
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

class ConstraintTest extends FlatSpec with Matchers with ArrayBackedStateBuilder {
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

  "variables" should "respect constraints" in {
    import uk.ac.ed.inf.mois.implicits._
    d1 must (_ >= 0)

    intercept[ConstraintViolation] {
      d1 := -1.0
      d1.assertConstraints
    }

    d2 must (_ >= 0) and (_ <= 2)
    intercept[ConstraintViolation] {
      d2 := -1.0
      d2.assertConstraints
    }

    intercept[ConstraintViolation] {
      d2 := 3.0
      d2.assertConstraints
    }
  }
}
