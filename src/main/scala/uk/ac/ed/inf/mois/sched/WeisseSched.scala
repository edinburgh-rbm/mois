/*
 *  MOIS: Weisse Scheduler
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
package uk.ac.ed.inf.mois.sched

import uk.ac.ed.inf.mois.{ProcessGroup, Scheduler, AdaptiveTimestep, VarMeta}
import uk.ac.ed.inf.mois.syntax._

import spire.algebra.Rig
import spire.math.{abs, min, max}
import spire.implicits._

class WeisseScheduler(
  val tolerance: Double = 1e-1,
  val rho: Double = 0.9,
  val dt_min: Double = 1e-8,
  val dt_max: Double = 1e0,
  val threshold: Double = 1e-4)
    extends Scheduler with WeisseAdaptiveTimestep {

  private val rig = Rig[Double]

  def apply(t: Double, tau: Double, group: ProcessGroup) = {
    val x0: Array[Double] = group.state.get[Double]
    val dx = Array.fill(x0.size)(rig.zero)

    val dt = calculateInitialTimestep(tau)

    for (child <- group.processes) {
      group.state >>> child.state
      child.step(t, tau)
      val doubles: Array[Double] = child.state.get[Double]
      // XXX should propagate all non-double vars here
      // XXXXX inefficient!!!
      var i = 0
      while (i < doubles.size) {
        val j = group.state.getMeta(rig) indexOf child.state.getMeta(rig)(i)
        dx(j) += doubles(i) - x0(j)
        i += 1
      }
    }

    calculateNewTimestep(x0, dx, t, dt, group)
  }
}

trait WeisseAdaptiveTimestep extends AdaptiveTimestep {
  val tolerance: Double
  val rho: Double
  val dt_min: Double
  val dt_max: Double
  val threshold: Double

  def calculateInitialTimestep(tau: Double) =
    if (tau > dt_max) dt_max else tau

  def calculateNewTimestep(
    x0: Array[Double], dx: Array[Double],
    t: Double, dt: Double, group: ProcessGroup
  ) = {
    // XXX This is a workaround for the new autonomous form
    // of time, where it is a first-class variable. we set the
    // change in time directly here, to zero, so that it does
    // not perturb the adaptive timestep calculation
    val timeIdx = 
      group.state.getMeta[Double] indexOf VarMeta("sim:t", Rig[Double])
    dx(timeIdx) = 0

    // use absolute error for variables near 0 and relative for others
    def estimateError(i: Int): Double = {
      val x0_i = abs(x0(i))
      val dx_i = abs(dx(i))
      if (x0_i > threshold) // relative error
        dx_i/x0_i
      else
        dx_i
    }
    val err = (0 until x0.size).map(estimateError(_)).max

    val new_dt = max(dt_min, min(dt_max, rho * dt * tolerance / err))

    // return
    if (err < tolerance) {
      // all good, update group variables
      // make sure to put the right time
      dx(timeIdx) = dt
      group.state := x0 + dx
      (t+dt, new_dt)
    } else {
      // error too large, try again. attention tail recursion!
      if (dt == new_dt)
        throw new Exception(s"Minimum time step reached. err = ${err}")
      apply(t, new_dt, group)
    }
  }
}
