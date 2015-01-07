/*
 *  MOIS: Weisse Scheduler Test
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

import spire.implicits._
import uk.ac.ed.inf.mois.implicits._

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.{Model, ProcessGroup, State}
import uk.ac.ed.inf.mois.ode.{ODE, Apache}
import uk.ac.ed.inf.mois.sched.WeisseScheduler

class CoupledOscillator(w: Double, k: Double) extends ODE[Double, Double] with Apache {
  annotate("description", "Coupled Oscillator")

  val x1 = Double("c:x1")
  val x2 = Double("c:x2")
  val x3 = Double("c:x3")
  val x4 = Double("c:x4")

  d(x1) := x2
  d(x2) := w * (x1 - 100) - k * (x1 - x3)
  d(x3) := x4
  d(x4) := w * (x3 - 100) - k * (x3 - x1)
}

class CoupledOscillatorModel extends Model {
  val w = -1.0
  val k = 0.5
  val process = new CoupledOscillator(w, k)
}

class CoupledOscillatorA(w: Double, k: Double) extends ODE[Double, Double] with Apache {
  annotate("description", "Coupled Oscillator A")

  val x1 = Double("d:x1")
  val x2 = Double("d:x2")
  val x3 = Double("d:x3")

  d(x1) := x2
  d(x2) := w * (x1 - 100) - k * (x1 - x3)
}

class CoupledOscillatorB(w: Double, k: Double) extends ODE[Double, Double] with Apache {
  annotate("description", "Coupled Oscillator B")

  val x1 = Double("d:x1")
  val x3 = Double("d:x3")
  val x4 = Double("d:x4")

  d(x3) := x4
  d(x4) := w * (x3 - 100) - k * (x3 - x1)
}

class CoupledOscillatorGroupModel extends Model {
  prefix("d", "http://edinburgh-rbm.github.io/mois/tests/")
  val w = -1.0
  val k = 0.5
  val process = new ProcessGroup {
    scheduler = new WeisseScheduler(tolerance=0.1)
  }
  process += new CoupledOscillatorA(w, k)
  process += new CoupledOscillatorB(w, k)
}

/** Run the two versions of the system of ODEs with the NaiveScheduler. */
class WeisseSchedulerTest extends FlatSpec with Matchers {

  // Use approximate equality in `should equal`
  val precision = 1e-3
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  import scala.math.abs

  def maxerr(s1: Array[Double], s2: Array[Double]) =
    (0 until s1.size).map( i => abs(1 - s1(i)/s2(i)) )
      .max

  def stateStr(s: State) = s.getMeta[Double].map(
    m => s.getVar[Double](m)).toList

  "coupled oscillator" should "give similar results directly as with weisse" in {
    val direct = new CoupledOscillatorModel
    val group = new CoupledOscillatorGroupModel

    direct.init(0)
    direct.run(0, 1, 1)
    direct.finish

    group.init(0)
    group.run(0, 1, 1)
    group.finish

/*
    println(stateStr(direct.process.state))
    println(stateStr(group.process.state))
 */

    val s1: Array[Double] = direct.process.state.get[Double]
    val s2: Array[Double] = group.process.state.get[Double]
    (maxerr(s1, s2) < 0.07) should  be (true)
  }

  ignore should "give right answers in the longer term too" in {
    val direct = new CoupledOscillatorModel
    val group = new CoupledOscillatorGroupModel

    direct.process.step(0, 10)
    group.process.step(0, 10)

    println(maxerr(direct.process.state.get[Double],
      group.process.state.get[Double]))
    (maxerr(direct.process.state.get[Double],
      group.process.state.get[Double]) < 0.07) should  be (true)
  }
}
