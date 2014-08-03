/*
 *  MOIS: Kick Scheduler Test
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

import uk.ac.ed.inf.mois.{Model, ProcessGroup, VarContainer, VarConversions, VarMeta}
import uk.ac.ed.inf.mois.sched.{KickScheduler}

class KickedModel extends Model {
  val w = Double("w") := -1.0
  val k = Double("k") := 0.5
  val process = new ProcessGroup("Coupled Oscillator Group") {
    scheduler = new KickScheduler(0.1)
  }
  process += new CoupledOscillatorA(w, k)
  process += new CoupledOscillatorB(w, k)
}

/** Run the two versions of the system of ODEs with the NaiveScheduler. */
class KickSchedulerTest extends FlatSpec with Matchers with VarConversions {

  // Use approximate equality in `should equal`
  val precision = 1e-4
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
    val group = new KickedModel

    direct.init(0)
    direct.run(0, 0.5)
    direct.finish

    group.init(0)
    group.run(0, 0.5)
    group.finish

/*
    println(direct.process.doubleVars.values.toList.sortBy(_.meta))
    println(group.process.doubleVars.values.toList.sortBy(_.meta))
    println(maxerr(direct.process, group.process))
 */
    (maxerr(direct.process, group.process) < 0.035) should  be (true)
  }
}
