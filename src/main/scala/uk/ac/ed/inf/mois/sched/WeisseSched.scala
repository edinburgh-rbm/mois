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

import uk.ac.ed.inf.mois.{ProcessGroup, Scheduler, Math}
import uk.ac.ed.inf.mois.{DoubleVar, VarConversions, VarMeta}

class WeisseScheduler(
  val tolerance: Double = 1e-1,
  val rho: Double = 0.9,
  val dt_min: Double = 1e-8,
  val dt_max: Double = 1e0,
  val threshold: Double = 1e-4)
    extends Scheduler with Math with VarConversions {

  // debugging
  var debug_err: DoubleVar = null
  override def init(group: ProcessGroup) {
    debug_err = group.Double(VarMeta("err"))
  }

  def apply(t: Double, tau: Double, group: ProcessGroup) = {
    val x0 = group.doubleVars.copy // all variables of the group
    val dx = group.doubleVars.zeros

    val dt = if (tau > dt_max) dt_max else tau

    for (child <- group.processes) {
      group >>> child
      child.step(t, tau)
      // XXX should propagate all non-double vars here
      dx +:= child.doubleVars - x0
    }

    // use absolute error for variables near 0 and relative for others
    def estimateError(v: DoubleVar) = {
      val x0_i = abs(x0(v.meta))
      val dx_i = abs(dx(v.meta))
      if (x0_i > threshold) // relative error
        dx_i/x0_i
      else
        dx_i
    }
    val err = x0.values.map(estimateError(_)).max
    debug_err := err

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
