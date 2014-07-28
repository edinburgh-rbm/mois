/*
 *  MOIS: MapReduce Scheduler
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

import scala.util.Random
import java.lang.Math.min

import uk.ac.ed.inf.mois.{Scheduler, BaseProcess, ProcessGroup}

abstract class MapReduceScheduler(step: Double) extends Scheduler {

  type Acc

  def accumulator: Acc

  def m(t: Double, dt: Double, group: ProcessGroup, proc: BaseProcess): BaseProcess
  def r(acc: Acc, proc: BaseProcess): Acc

  def before(t: Double, dt: Double, acc: Acc, group: ProcessGroup) = dt
  def after(t: Double, dt: Double, acc: Acc, group: ProcessGroup) = (t+dt, dt)

  def apply(t: Double, tau: Double, group: ProcessGroup) = {
    val acc = accumulator
    val dt = before(t, min(tau, step), acc, group)
    val procs = Random.shuffle(group.processes) map (m(t, dt, group, _))
    val results = procs.foldLeft(acc) (r _)
    after(t, dt, results, group)
  }
}
