/*
 *  MOIS: Scheduler
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

/** A Scheduler is used by [[ProcessGroup]] to run the member
  * processes. The important method is [[Scheduler.apply]] which
  * runs all of the processes for one time step.
  */
abstract class Scheduler {

  /** Perform any required initialisation.
    *
    * @param group the process group this scheduler is to run
    */
  def init(group: ProcessGroup) {}

  /** A single iteration of the scheduler must return the time
    * after the iteration, and a suggested time step for the next
    * iteration. The parameter `tau` is treated as advice and the
    * scheduler may run for a shorter time in order to be able to
    * adapt the step size to control errors.
    *
    * @param t the starting time for the step
    * @param tau the (suggested) size of the step
    * @param group the process group to run
    * @return time at end of step and new step size
    */
  def apply(t: Double, tau: Double, group: ProcessGroup): (Double, Double)
}

trait AdaptiveTimestep {
  def apply(t: Double, tau: Double, group: ProcessGroup): (Double, Double)

  def calculateInitialTimestep(tau: Double): Double

  def calculateNewTimestep(
    x0: VarMap[Double, DoubleVar], dx: VarMap[Double, DoubleVar],
    t: Double, dt: Double, group: ProcessGroup
  ): (Double, Double)
}
