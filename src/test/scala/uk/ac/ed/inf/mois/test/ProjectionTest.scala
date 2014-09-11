/*
 *  MOIS: Projection Test
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

import spire.math.Rational
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._
import uk.ac.ed.inf.mois.{ArrayBackedStateBuilder, Projection}

class ProjectionTest extends FlatSpec with Matchers {

  class S1 extends ArrayBackedStateBuilder {
    val b = Boolean("b")
    val u = Double("u")
    val v = Double("v")
    val w = Double("w")
    val x = Rational("x")
    val y = Rational("y")
    val z = Rational("z")
  }

  class S2 extends ArrayBackedStateBuilder {
    val v = Double("v")
    val x = Rational("x")
    val z = Rational("z")
    val b = Boolean("b")
  }


  "Projection" should "set values in the forward and reverse direction" in {
    val s1b = new S1
    val s1 = s1b.buildState
    s1b.initState(s1)

    val s2b = new S2
    val s2 = s2b.buildState
    s2b.initState(s2)

    val proj = Projection(s1, s2)

    s1b.u := 1.0
    s1b.v := 2.0
    s1b.w := 3.0
    s1b.x := 4
    s1b.y := 5
    s1b.z := 6
    s1b.b := true

    s1b.b.value should equal (true)
    s1b.u.value should equal (1.0)
    s1b.v.value should equal (2.0)
    s1b.w.value should equal (3.0)
    s1b.x.value should equal (4)
    s1b.y.value should equal (5)
    s1b.z.value should equal (6)

    s2b.b.value should equal (false)
    s2b.v.value should equal (0.0)
    s2b.x.value should equal (0)
    s2b.z.value should equal (0)

    proj.forward

    s2b.b.value should equal (true)
    s2b.v.value should equal (2.0)
    s2b.x.value should equal (4)
    s2b.z.value should equal (6)

    s2b.b := false
    s2b.v := s2b.v * 3

    val rats = s2.get[Rational]
    s2 := 2 *: rats

    proj.reverse

    s1b.b.value should equal (false)
    s1b.u.value should equal (1.0)
    s1b.v.value should equal (6.0)
    s1b.w.value should equal (3.0)
    s1b.x.value should equal (8)
    s1b.y.value should equal (5)
    s1b.z.value should equal (12)
  }
}
