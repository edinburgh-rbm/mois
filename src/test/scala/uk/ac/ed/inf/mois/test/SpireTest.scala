/*
 *  MOIS: Spire Test
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
import spire.algebra.{Ring, VectorSpace}
import spire.math.{Real, Rational}

import uk.ac.ed.inf.mois.SpireProcess

class SillyProcess[T](implicit ring: Ring[T]) extends SpireProcess[T] {
  val x = Var("hello")
  x annotate("who", "world")
  val y = Var("foo")
  y annotate("what", "bar")

  def step(x: Vector[T], t: Double, tau: Double)(implicit
    vs: VectorSpace[Vector[T], T] // needed for the - operation
  ) = {
    /* just do something silly like print out the state X */
    val m0 = variables(0)
    val v0 = x(0)
    for (i <- 0 until x.size) {
      val m = variables(i)
      val v = x(Index(m))
      println(s"x($i) = $m = $v")
    }

    // we can reference the vector like this too
    require(x(y) == x(Index("foo")), "...")

    // do something to the state vector
    x :/ 0.5
  }
}

class SpireTest extends FlatSpec with Matchers {
  "foo" should "bar" in {

    val p = new SillyProcess[Real]
    p.x := 1.0
    p.y := 2.0
    println(p.step(p.initialValues, 0, 1))

    (1) should be (1)
  }
}
