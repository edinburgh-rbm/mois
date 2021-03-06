/*
 *  MOIS: Process Test
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

import uk.ac.ed.inf.mois.Process
import uk.ac.ed.inf.mois.implicits._

import org.scalatest.{FlatSpec, Matchers}

class ProcessTest extends FlatSpec with Matchers {

  class P extends Process {
    prefix("ex", "http://example.edu/")
    val x1 = Double("ex:x1")
    val x2 = Double("ex:x2")
    val x3 = Boolean("gg:x3")
    override def step(t: Double, tau: Double) {
      x1 := t + x1 * x2 * tau
      if (t % 2 != 0) x3 := !x3
      else x2 := x2 * x2
    }
  }

  it should "run and do arithmetic" in {
    val p = new P
    p.init(0)

    p.x1 := 2.0
    p.x2 := 3.0
    p.x3 := true

    p(0, 1)

    p.x1.value should be (6.0)
    p.x2.value should be (9.0)
    p.x3.value should be (true)

    p(1, 1)
    p.x1.value should be (55.0)
    p.x2.value should be (9.0)
    p.x3.value should be (false)
  }

  it should "expand CURIEs in variable names" in {
    val p = new P
    p.init(0)

    p.x1.meta.identifier should equal ("http://example.edu/x1")
    p.x3.meta.identifier should equal ("gg:x3")
  }
}
