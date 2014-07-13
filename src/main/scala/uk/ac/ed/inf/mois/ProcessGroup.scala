/*
 *  MOIS: Process Group
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
package uk.ac.ed.inf.mois

import scala.collection.mutable

/** A `ProcessGroup` is a list of `Process`es and a `Scheduler`.
  * It presents the same interface as a `Process` and so hierarchies
  * of them can be built.
  */
class ProcessGroup(val name: String) extends BaseProcess {

  override def stringPrefix = "ProcessGroup"

  var processes = mutable.ArrayBuffer.empty[BaseProcess]
  var scheduler: Scheduler = null

  /** The += operator adds a process to the group. */
  def += (proc: BaseProcess) = {
    // merge vars to this (lhs) from proc (rhs)
    leftMerge(proc)

    processes += proc

    this
  }

  /** The -= operator removes a process from the group. */
  def -= (proc: BaseProcess) = {
    // TODO: needed for process migration. Keeping state
    // coherent is important here
    this
  }

  /** The `step` method of the `Process` interface calls the
    * `Scheduler` on the list of processes together with the group
    * state table and time parameters.
    */
  def step(t0: Double, tau: Double) {
    scheduler.init(this)
    var t = t0
    while (t < t0+tau) {
      t += scheduler(t, tau, this)
      for (sh <- stepHandlers)
        sh.handleStep(t, this)
    }
  }

  /** Override the `apply` method because we take on responsibility
    * for calling the step handlers.
    */
  @inline override def apply(t: Double, tau: Double) = step(t, tau)
}
