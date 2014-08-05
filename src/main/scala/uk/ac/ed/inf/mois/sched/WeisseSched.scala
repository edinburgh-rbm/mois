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

import uk.ac.ed.inf.mois.{ProcessGroup, Scheduler, AdaptiveTimestep, Math}
import uk.ac.ed.inf.mois.{DoubleVar, VarMapConversions, VarMap, VarMeta}

class WeisseScheduler(
  val tolerance: Double = 1e-1,
  val rho: Double = 0.9,
  val dt_min: Double = 1e-8,
  val dt_max: Double = 1e0,
  val threshold: Double = 1e-4)
    extends Scheduler with WeisseAdaptiveTimestep with VarMapConversions {

  def apply(t: Double, tau: Double, group: ProcessGroup) = {
    val x0 = group.doubleVars.copy // all variables of the group
    val dx = group.doubleVars.zeros

    val dt = calculateInitialTimestep(tau)

    for (child <- group.processes) {
      group >>> child
      child.step(t, dt)
      // XXX should propagate all non-double vars here
      dx += child.doubleVars - x0
    }
    calculateNewTimestep(x0, dx, t, dt, group)
  }
}

trait WeisseAdaptiveTimestep extends AdaptiveTimestep with VarMapConversions with Math {
  val tolerance: Double
  val rho: Double
  val dt_min: Double
  val dt_max: Double
  val threshold: Double

  def calculateInitialTimestep(tau: Double) =
    if (tau > dt_max) dt_max else tau

  def calculateNewTimestep(
    x0: VarMap[Double, DoubleVar], dx: VarMap[Double, DoubleVar],
    t: Double, dt: Double, group: ProcessGroup
  ) = {
    // use absolute error for variables near 0 and relative for others
    def estimateError(v: DoubleVar): Double = {
      val x0_i = abs(x0(v.meta).value)
      val dx_i = abs(dx(v.meta).value)
      if (x0_i > threshold) // relative error
        dx_i/x0_i
      else
        dx_i
    }
    val err = x0.values.map(estimateError(_)).max

    val new_dt = max(dt_min, min(dt_max, rho * dt * tolerance / err))

    // return
    if (err < tolerance) {
      // all good, update group variables
      group.doubleVars <<< (x0 + dx)
      (t+dt, new_dt)
    } else {
      // error too large, try again. attention tail recursion!
      if (dt == new_dt)
        throw new Exception(s"Minimum time step reached. err = ${err}")
      apply(t, new_dt, group)
    }
  }
}
