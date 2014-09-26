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
import uk.ac.ed.inf.mois.{ArrayBackedStateBuilder, ConstraintViolation}

class ConstraintTest extends FlatSpec with Matchers with ArrayBackedStateBuilder {
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

    d1.checkConstraints should be (true)

    d1 := -1.0
    d1.checkConstraints should be (false)
    intercept[ConstraintViolation] {
      d1.assertConstraints
    }

    d2 must (_ >= 0) and (_ <= 2)
    d2 := -1.0
    d2.checkConstraints should be (false)

    d2 := 3.0
    d2.checkConstraints should be (false)

    d2 := 0.0
    d2.checkConstraints should be (true)
  }

  it should "respect bounds" in {
    import spire.implicits._
    import uk.ac.ed.inf.mois.implicits._

    i1 gte(0)
    i1 lte(5)

    i1 := -1
    i1.value should equal (0)
    i1 := 10
    i1.value should equal (5)
    i1 := 3
    i1.value should equal (3)

    i2 nonnegative()
    i2 := -1
    i2.value should equal (0)
    i2 := 10
    i2.value should equal (10)
  }
}
