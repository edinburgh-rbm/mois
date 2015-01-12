/*
 *  MOIS: Rosenbrock Stiff Integrator Test
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

import scala.language.reflectiveCalls
import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.Math
import uk.ac.ed.inf.mois.ode.{ODE, Rosenbrock}
import uk.ac.ed.inf.mois.reaction.DeterministicReactionNetwork
import spire.math.Jet
import spire.implicits._

/** The prototypical stiff system. The solution with initial
  * values. [u0, v0] = [1, 0]
  *
  *  u = 2 * exp(-t) - exp(-1000 * t)
  *  v = -exp(-t) + exp(-1000 * t)
  * 
  * This makes non-stiff integrators have conniptions because
  * they adjust the time-step for the exp(-1000t) term which
  * DOESN'T MATTER
  */
class StiffSystem extends ODE[Double, Jet[Double]] with Rosenbrock {
  val u = Double("u") default(1)
  val v = Double("v")
  d(u) := u*998 +v*1998
  d(v) := u*(-999) - v*1999

  def solution(t: Double): (Double, Double) = {
    import scala.math.exp
    val u = 2.0 * exp(-1.0 * t) - exp(-1000.0 * t)
    val v = -1.0 * exp(-1.0 * t) + exp(-1000.0 * t)
    (u, v)
  }
}

class StiffReaction extends DeterministicReactionNetwork[Double, Jet[Double]]
    with Rosenbrock {
  val A = Species("A")
  val B = Species("B")
  val C = Species("C")
  reactions(
    A --> B at 0.04,
    B + B --> C + B at 3e7,
    B + C --> A + C at 1e4
  )
}


class RosenbrockTest extends FlatSpec with Matchers {

  // Use approximate equality in `should equal`
  val precision = 1e-4
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "rosenbrock" should "work with normal ODEs" in {
    val p = new StiffSystem
    p.init(0)
    p.step(0, 1)

    val (u1, v1) = p.solution(1)
    p.u.value should equal (u1)
    p.v.value should equal (v1)

    p.step(1, 9)
    val (u10, v10) = p.solution(10)
    p.u.value should equal (u10)
    p.v.value should equal (v10)

    p.step(10, 90)
    val (u100, v100) = p.solution(100)
    p.u.value should equal (u100)
    p.v.value should equal (v100)

    p.step(100, 900)
    val (u1000, v1000) = p.solution(1000)
    p.u.value should equal (u1000)
    p.v.value should equal (v1000)
  }

  it should "also work with reactions" in {
    val p = new StiffReaction
    p.init(0)
    println(p.vars)
    println(p.funs.map(x => x()).toSeq)
    p.step(0, 1)
    println(p.vars)
    p.step(1, 1000)
    println(p.vars)
  }
}
