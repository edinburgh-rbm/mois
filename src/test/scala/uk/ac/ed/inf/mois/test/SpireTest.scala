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

import scala.collection.mutable
import spire.algebra.{Field, Ring, VectorSpace}

import spire.implicits._
import spire.math.{Numeric, Real, Rational, Number}


// import uk.ac.ed.inf.mois.SpireProcess

// class SillyProcess extends SpireProcess {
//   val x = Double("hello")
//   x annotate("who", "world")
//   val y = Double("foo")
//   y annotate("what", "bar")
//   val z = BigInt("Big")
//   val b = Boolean("Bool")
// }

// abstract class Integrator[T] {

//   def apply(x0: Vector[T], t0: Double, tau: Double): Vector[T]

//   def x (d: Double): Integrator[T] = {
//     val old = this
//     new Integrator[T] {
//       def apply(x0: Vector[T], t0: Double, tau: Double) = {
//         old(x0, t0, tau/2)
//       }
//     }
//   }

//   def compose(rhs: Integrator[T]) = {
//     val lhs = this
//     new Integrator[T] {
//       def apply(x0: Vector[T], t0: Double, tau: Double) = {
//         lhs(rhs(x0, t0, tau), t0, tau)
//       }
//     }
//   }
// }

// case class ForwardEuler[T](f: Integrable[T])(implicit vs: VectorSpace[Vector[T], T])
//     extends Integrator[T] {
//   def apply(x0: Vector[T], t0: Double, tau: Double) = {
//     val dx = f.derivative(x0, t0) :/ (1/tau)
//     x0 + dx
//   }
// }

// /**
//   */
// abstract class Integrable[T] {
//   def derivative(x: Vector[T], t: Double): Vector[T]
// }

// class SpireTest extends FlatSpec with Matchers {
//   "foo" should "bar" in {

//     val p = new SillyProcess
//     p.init(p.buildState, 0)
//     p.x := 1.0
//     p.y := 2.0
//     println(s"${p.x}, ${p.y}")

//     val f = new Integrable[Double] {
//       def derivative(x: Vector[Double], t: Double) = Vector.fill(x.size)(1.0)
//     }

//     val i2 = (ForwardEuler(f) x 0.5) compose (ForwardEuler(f) x 0.5)

//     println(i2(Vector(0.0), 0, 10))

//     (1) should be (1)
//   }
// }
