/*
 *  MOIS: Hamiltonian Process Test
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

import java.lang.Math.{cos, PI}
import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.HamiltonianProcess

case class Pendulum(m: Double, l: Double) extends HamiltonianProcess("Pendulum") {
  val q = Double("ex:q")
  val p = Double("ex:p")
  val g = 9.81
  H(Seq(q), Seq(p)) := (p*p)/(2*m*l*l) + m*g*l*(1 - cos(q))
}


class HamiltonianProcessTest extends FlatSpec with Matchers {
  "hamiltonian pendulum" should "give correct results" in {
    // Use approximate equality in `should equal`
    val precision = 1e-4
    implicit val doubleEquality =
      TolerantNumerics.tolerantDoubleEquality(precision)

    val pendulum = new Pendulum(1, 1)
    import pendulum._

    q := PI/4

    pendulum.step(0, 10)

    println(p)
    println(q)
    1 should not be 0
  }
}
