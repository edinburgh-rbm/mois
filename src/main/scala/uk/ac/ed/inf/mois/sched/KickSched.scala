/*
 *  MOIS: Kick Scheduler
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

import scala.collection.mutable

import uk.ac.ed.inf.mois.{BaseProcess, ProcessGroup, Scheduler, Math}
import uk.ac.ed.inf.mois.{AdaptiveTimestep}
import uk.ac.ed.inf.mois.{DoubleVar, VarMapConversions, VarMap, VarMeta}

abstract class KickMethod
    extends Scheduler with AdaptiveTimestep with Math with VarMapConversions {

  def apply(t: Double, tau: Double, group: ProcessGroup) = {
    val dt = calculateInitialTimestep(tau)
    kickStep(t, dt, group)
  }

  def kickStep(t: Double, dt: Double, group: ProcessGroup) = {
//    println("")
//    println(s"KickSched $t + $dt")

//    println(s"Drifting...")
    // 1. allow processes to drift for the whole time
    val x_tau = mutable.Map.empty[BaseProcess, VarMap[Double, DoubleVar]]
    for (child <- group.processes) {
      group >>> child
      child.step(t, dt)
      x_tau += child -> child.doubleVars.copy
    }

//    println(s"Calculating partials")
    // 2. approximate partial derivatives of child at t + tau/2
    val partials = mutable.Map.empty[BaseProcess, mutable.Map[VarMeta, VarMap[Double, DoubleVar]]]
    for (child <- group.processes) {
      partials += child -> child.partialDerivatives(t, dt/2)
    }

//    println("Drifting for half the time")
    // 3. integrate and let drift for half the time
    for (child <- group.processes) {
      group >>> child
      child.step(t, dt/2)
    }

//    println("Kicking")
    // 4. apply the kick
    for (child <- group.processes) {
      val partial_c = partials(child)
//      println(child)
//      for (v <- partial_c.keys.toList) {
//        val pp = partial_c(v).values.toList.sortBy(_.meta)
//        println(s"\td/d${v.identifier} = $pp")
//      }
      val kick = child.doubleVars.zeros
      for {
        other <- group.processes if other != child
        (mx, x) <- x_tau(other)
        (mv, v) <- child.doubleVars if v != x
      } kick(mv) += partial_c(mx)(mv).value * x.value / 4 //2
//      println(child)
//      for (v <- child.doubleVars.values.toList.sortBy(_.meta)) {
//        val kv = kick(v)
//        println(s"\t$v\t$kv")
//      }
      child.doubleVars += kick
    }

//    println("Drifting the rest of the time")
    // 5. integrate and let drift for the rest of the time
    val x0 = group.doubleVars.copy // all variables of the group
    val dx = group.doubleVars.zeros // accumulate the change
    for (child <- group.processes) {
      child.step(t+dt/2, dt/2)
      dx += (child.doubleVars - x0)
    }
    calculateNewTimestep(x0, dx, t, dt, group)
  }
}


class KickScheduler(val dt: Double) extends KickMethod with AdaptiveTimestep with VarMapConversions {
  def calculateInitialTimestep(tau: Double) = dt
  def calculateNewTimestep(
    x0: VarMap[Double, DoubleVar], dx: VarMap[Double, DoubleVar],
    t: Double, dt: Double, group: ProcessGroup
  ) = {
    group.doubleVars <<< (x0 + dx)
    (t+dt, dt)
  }
}

class AdaptiveKickScheduler(
  val tolerance: Double = 1e-1,
  val rho: Double = 0.9,
  val dt_min: Double = 1e-8,
  val dt_max: Double = 1e0,
  val threshold: Double = 1e-4)
    extends KickMethod with WeisseAdaptiveTimestep {

  override def apply(t: Double, tau: Double, group: ProcessGroup) = {
    val dt = calculateInitialTimestep(tau)
    kickStep(t, dt, group)
  }
}
