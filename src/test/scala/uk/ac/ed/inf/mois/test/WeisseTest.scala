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

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.{Model, ODE, ProcessGroup}
import uk.ac.ed.inf.mois.{VarContainer, VarConversions, VarMeta}
import uk.ac.ed.inf.mois.sched.{WeisseScheduler}

class CoupledOscillator(w: Double, k: Double)
    extends ODE("Coupled Oscillator") {
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
  val w = Double("w") := -1.0
  val k = Double("k") := 0.5
  val process = new CoupledOscillator(w, k)
}

class CoupledOscillatorA(w: Double, k: Double)
    extends ODE("Coupled Oscillator A") {
  val x1 = Double("d:x1")
  val x2 = Double("d:x2")
  val x3 = Double("d:x3")

  d(x1) := x2
  d(x2) := w * (x1 - 100) - k * (x1 - x3)
}

class CoupledOscillatorB(w: Double, k: Double)
    extends ODE("Coupled Oscillator B") {
  val x1 = Double("d:x1")
  val x3 = Double("d:x3")
  val x4 = Double("d:x4")

  d(x3) := x4
  d(x4) := w * (x3 - 100) - k * (x3 - x1)
}

class CoupledOscillatorGroupModel extends Model {
  val w = Double("w") := -1.0
  val k = Double("k") := 0.5
  val process = new ProcessGroup("Coupled OscillatorGroup") {
    scheduler = new WeisseScheduler
  }
  process += new CoupledOscillatorA(w, k)
  process += new CoupledOscillatorB(w, k)
}

/** Run the two versions of the system of ODEs with the NaiveScheduler. */
class WeisseSchedulerTest extends FlatSpec with Matchers with VarConversions {

  // Use approximate equality in `should equal`
  val precision = 1e-3
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  import scala.math.abs
  implicit def stringToMeta(s: String) = VarMeta(s)

  def maxerr(p1: VarContainer, p2: VarContainer) =
    Seq("x1", "x2", "x3", "x4")
      .map(v => abs(1-p1.doubleVars("c:" + v)/p2.doubleVars("d:" + v)))
      .max

  "coupled oscillator" should "give similar results directly as with weisse" in {
    val direct = new CoupledOscillatorModel
    val group = new CoupledOscillatorGroupModel

    direct.process.step(0, 1)
    group.process.step(0, 1)

    (maxerr(direct.process, group.process) < 0.07) should  be (true)
  }

  ignore should "give right answers in the longer term too" in {
    val direct = new CoupledOscillatorModel
    val group = new CoupledOscillatorGroupModel

    direct.process.step(0, 10)
    group.process.step(0, 10)

    println(maxerr(direct.process, group.process))
    (maxerr(direct.process, group.process) < 0.07) should  be (true)
  }
}
