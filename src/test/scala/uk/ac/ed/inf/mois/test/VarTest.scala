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

import uk.ac.ed.inf.mois.{VarContainer, VarConversions, ConstraintViolation}

class VarTest extends FlatSpec with Matchers
    with VarContainer with VarConversions {

  // Use approximate equality in `should equal` for doubles
  val precision = 1e-8
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "state variables" should "support arithmetic operations" in {
    val ir1 = Int("ex:ir1") := 1
    val ir2 = Int("ex:ir2") := 2
    (ir1 + ir2) should be (3)
  }

  it should "not care too much about types for arithmetic" in {
    val r1 = Float("ex:float")
    r1 += 1
  }


  it should "support difference" in {
    val r1_0 = Double("ex:r1") := 0.5
    val r1_1 = Double("ex:r1") := 0.8
    val r2 = Double("ex:r2") := 1.0

    val dr1 = r1_1 - r1_0
    dr1 should equal (0.0)

    val dr2 = r2 - r1_1
    dr2 should equal (0.2)
  }

  it should "support subtraction with doubles" in {
    val v = Double("ex:v")
    (v - 1.0) should equal (-1.0)
  }

  it should "respect constraints" in {
    val r1 = Double("ex:r1")
    r1 must (_ >= 0)

    r1 := 0
    r1 := 1
    intercept[ConstraintViolation] {
      r1 := -1.0
    }

    val r2 = Double("ex:r2")
    r2 must (_ <= 2)
    r2 := 0
    r2 := 2
    intercept[ConstraintViolation] {
      r2 := 3.0
    }

    val r3 = Double("ex:r3")
    r3 must (_ >= 0) and (_ <= 2)
    r3 := 1
    intercept[ConstraintViolation] {
      r3 := -1.0
    }
    intercept[ConstraintViolation] {
      r3 := 3.0
    }
  }

  it should "suport modifiers" in {
    val x = Double("x")
    x gte(0)
    x lte(100)
    x := -1
    (x.value) should equal (0.0)
    x := 101
    (x.value) should equal (100.0)
  }

  it should "have unambiguous keys" in {
    val v1 = Int("a")
    val v2 = Int("a")
    v1.meta should be (v2.meta)

    val v3 = Int("b")
    v1.meta should not be (v3.meta)

    val v4 = Int("a")
    v4 := 1
    v1.meta should be (v4.meta)
  }

  it should "have natural syntax for numerical operations" in {
    val x1 = Double("ex:x1")
    val x2 = Double("ex:x2") := 1

    x1 := 2
    x1.value should be (2)

    x2 := 2 + 2*x1
    x2.value should be (6)

    val dx = x2 - x1
    (2 * dx) should be (8)

    x1 += dx
    x1.value should be (x2.value)

    (x1 - dx) should be (x2 - dx)
  }

  it should "be copyable" in {
    val x1 = Double("ex:x1") := 5
    val x2 = x1.copy
    x1 should not equal (x2)
    x1.value should equal (x2.value)
    x2 := 1
    x1.value should not equal (x2.value)
  }

  "containers of vars" should "support bulk operations (for doubles)" in {
    class VC extends VarContainer {
      val x = Double("x")
      val y = Double("y")
    }

    val vc1 = new VC
    val vc2 = new VC
    vc1.x := 10
    vc2.y := 20

    import vc1.{x,y} // just for convenient access
    val v1 = vc1.doubleVars
    val v2 = vc2.doubleVars

    val v3 = v1 + v2
    v1(x).value should be (10)
    v1(y).value should be (0)
    v2(x).value should be (0)
    v2(y).value should be (20)
    v3(x).value should be (10)
    v3(y).value should be (20)

    v1 += v3
    v1(x).value should be (20)
    v1(y).value should be (20)

    v2 /= v1
    v2(x).value should be (0)
    v2(y).value should be (1)

    v3 -= v2
    v3(x).value should be (10)
    v3(y).value should be (19)
  }
}
