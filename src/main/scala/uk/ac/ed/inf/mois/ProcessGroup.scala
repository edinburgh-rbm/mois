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

/** A `ProcessGroup` is a list of [[Process]]es and a [[Scheduler]].
  * It presents the same interface as a [[Process]] and so hierarchies
  * of them can be built.
  *
  * It is typically created like so
  *
  * {{{
  * import uk.ac.ed.inf.mois.ProcessGroup
  * import uk.ac.ed.inf.mois.sched.WeisseScheduler
  *
  * val group = new ProcessGroup
  * group.scheduler = new WeisseScheduler(tolerance=0.1)
  *
  * group += new SomeSortOfProcess
  * group += new SomeOtherSortOfProcess
  * ...
  * }}}
  *
  * @param name the name of the process
  */
class ProcessGroup extends Process {

  /** The list of child processes */
  val processes = mutable.ArrayBuffer.empty[Process]

  /** The scheduler */
  var scheduler: Scheduler = null

  /** The += operator adds a process to the group. */
  def += (proc: Process) = {
    // merge the state builder instance from that process to this
    merge(proc)
    processes += proc
    this
  }

  /** (Unimplemented) The -= operator removes a process from the group. */
  def -= (proc: Process) = {
    // TODO: needed for process migration. Keeping state
    // coherent is important here
    throw new Exception("unimplemented")
  }

  /** The [[Process.step]] method of the [[Process]] interface calls
    * the [[Scheduler]] on the list of processes together with the group
    * state table and time parameters.
    */
  override def step(t0: Double, tau: Double) {
    _step(t0, tau, t0+tau)
  }

  private def _step(t0: Double, tau: Double, end: Double) {
    val (t, dt) = scheduler(t0, tau, this)
    for (sh <- stepHandlers)
      sh.handleStep(t, this)
    if (t < end)
      _step(t, dt, end)
  }

  /** Override the [[Process.apply]] method because we take on
    * responsibility for calling the step handlers.
    */
  @inline override def apply(t: Double, tau: Double) = {
    step(t, tau)
  }

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
