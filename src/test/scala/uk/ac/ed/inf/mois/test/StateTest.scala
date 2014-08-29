/*
 *  MOIS: State Test
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

import spire.algebra.Rig
import spire.implicits._
import uk.ac.ed.inf.mois.{State, StateBuilder}

class StateTest extends FlatSpec with Matchers {

  class DummyState extends StateBuilder {
    val x = Int("x")
    val y = Int("y")
    val u = Double("u")
    val v = Double("v")
    val b = Boolean("b")
    val state = buildState
    initStateIndices(state)
  }

  "a state" should "support adding and retrieving variables" in {
    val s = new DummyState

    s.x.value should equal (0)
    s.y.value should equal (0)

    s.x := 2
    s.y := 3

    s.x+s.y should equal(5)
  }

  it should "support retrieving all variables of a given type" in {
    val s = new DummyState
    val ints: Array[Int] = s.state
    val doubles: Array[Int] = s.state
  }
}
