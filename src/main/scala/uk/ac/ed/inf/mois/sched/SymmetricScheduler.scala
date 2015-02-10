/*
 *  MOIS: Symmetric Composition Scheduler
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

import scala.math.min
import scala.collection.mutable
import uk.ac.ed.inf.mois.{Process, ProcessGroup, Projection, Scheduler}

class SymmetricScheduler(step: Double) extends CompositionScheduler(step) {
  override def processes(group: ProcessGroup) =
    group.processes ++ group.processes.reverse
  override def apply(t: Double, tau: Double, group: ProcessGroup) = {
    val h = min(tau, step)
    for (child <- processes(group)) {
      val proj = projections(child)
      proj.forward
      child(t, h/2)
      proj.reverse
    }
    (t+h, h)
  }
}
