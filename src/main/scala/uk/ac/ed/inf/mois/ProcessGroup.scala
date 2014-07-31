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

/** A `ProcessGroup` is a list of [[BaseProcess]]es and a [[Scheduler]].
  * It presents the same interface as a [[BaseProcess]] and so hierarchies
  * of them can be built.
  *
  * It is typically created like so
  *
  * {{{
  * import uk.ac.ed.inf.mois.ProcessGroup
  * import uk.ac.ed.inf.mois.sched.WeisseScheduler
  *
  * val group = new ProcessGroup("my group")
  * group.scheduler = new WeisseScheduler(tolerance=0.1)
  *
  * group += new SomeSortOfProcess
  * group += new SomeOtherSortOfProcess
  * ...
  * }}}
  *
  * @param name the name of the process
  */
class ProcessGroup(val name: String) extends BaseProcess {

  override def stringPrefix = "ProcessGroup"

  /** The list of child processes */
  var processes = mutable.ArrayBuffer.empty[BaseProcess]
  /** The scheduler */
  var scheduler: Scheduler = null

  /** The += operator adds a process to the group. */
  def += (proc: BaseProcess) = {
    // merge vars to this (lhs) from proc (rhs)
    leftMerge(proc)

    processes += proc

    this
  }

  /** (Unimplemented) The -= operator removes a process from the group. */
  def -= (proc: BaseProcess) = {
    // TODO: needed for process migration. Keeping state
    // coherent is important here
    throw new Exception("unimplemented")
  }

  /** The [[BaseProcess.step]] method of the [[BaseProcess]] interface calls
    * the [[Scheduler]] on the list of processes together with the group
    * state table and time parameters.
    */
  def step(t0: Double, tau: Double) {
    _step(t0, tau, t0+tau)
  }

  private def _step(t0: Double, tau: Double, end: Double) {
    val (t, dt) = scheduler(t0, tau, this)
    for (sh <- stepHandlers)
      sh.handleStep(t, this)
    if (t < end)
      _step(t, dt, end)
  }

  /** Override the [[BaseProcess.apply]] method because we take on
    * responsibility for calling the step handlers.
    */
  @inline override def apply(t: Double, tau: Double) = step(t, tau)

  /** Override init hook by calling all of our children's before our own */
  override def init(t: Double) {
    scheduler.init(this)
    for (p <- processes) p.init(t)
    super.init(t)
  }

  /** Override reset hook by calling all of our children's before our own */
  override def reset(t: Double) {
    for (p <- processes) p.reset(t)
    super.reset(t)
  }

  /** Override finish hook by calling all of our children's before our own */
  override def finish {
    for (p <- processes) p.finish
    super.finish
  }
}
