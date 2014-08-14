/*
 *  MOIS: Python Test
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

import uk.ac.ed.inf.mois.PythonProcess

class S extends PythonProcess {
  val x = Double("ex:x")
  val y = Double("ex:y")
  val z = Double("ex:z")
  val _py = py
}


class PythonProcessTest extends FlatSpec with Matchers {
  // Use approximate equality in `should equal`
  val precision = 1e-3
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "a python process" should "receive and set variables" in {
    val s = new S
    s.init(0)

    import s._

    s.x := 1.0
    s._py(z) := Python("basic_tests").incx(x, y)
    s.step(0, 0.1)
    s.z.value should equal (2.0)
    assert(true)
  }

  it should "error when we ask for a nonexistent function" in {
    object nonexistent extends S {
      _py(x) := Python("basic_tests").nonexistent()
    }
    intercept[IllegalArgumentException] {
      nonexistent.step(0, 0.1)
    }
  }

  it should "error when too many or too few values are returned" in {
    object many extends S {
      _py(x) := Python("basic_tests").tooMany()
    }
    intercept[IllegalArgumentException] {
      many.step(0, 0.1)
    }

    object few extends S {
      _py(x,y,z) := Python("basic_tests").tooFew()
    }
    intercept[IllegalArgumentException] {
      few.step(0, 0.1)
    }
  }


  it should "be happy returning no values" in {
    object none extends S {
      _py() := Python("basic_tests").none()
    }
    none.step(0, 0.1)
  }

  it should "be happy with one return value" in {
    object one extends S {
      _py(x) := Python("basic_tests").one()
    }
    one.step(0, 0.1)
    one.x.value should equal(1.0)
  }

  it should "break properly when python func has wrong signature" in {
    object err extends S {
      _py(x) := Python("basic_tests").error()
    }
    intercept[IllegalArgumentException] {
      err.step(0, 0.1)
    }
  }

  it should "be able to access the time" in {
    object time extends S {
      _py(x,y) := Python("basic_tests").time()
    }
    time.step(2,3)
    time.x.value should equal (2.0)
    time.y.value should equal (3.0)
  }
}
