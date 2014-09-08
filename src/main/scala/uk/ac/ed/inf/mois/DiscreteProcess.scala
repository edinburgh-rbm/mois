/*
 *  MOIS: Discrete Process
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
import scala.reflect.ClassTag
import spire.algebra.Rig

abstract class DiscreteProcess[T: ClassTag](implicit rig: Rig[T]) extends Process {
  type Func = () => T

  /** Configurable time step size. This is a discrete time process but
    * nevertheless might be integrated with a continuous time process.
    * This stepSize is the amount of continuous, real, time that a discrete
    * time step can be set to take. Defaults to 1.0, naturally.
    */
  val stepSize = 1.0

  private val _vars = mutable.ArrayBuffer.empty[Var[T]]
  private lazy val vars = _vars.toArray
  private val _funcs = mutable.ArrayBuffer.empty[Func]
  private lazy val funcs = _funcs.toArray

  protected class Next(val v: Var[T]) {
    def := (e: => T): Unit = addNext(v, () => e)
  }

  protected def addNext(v: Var[T], f: Func) {
    _vars += v
    _funcs += f
  }

  @inline final def next(v: Var[T]) = new Next(v)
  @inline final def n(v: Var[T]) = new Next(v)

  override def step(t0: Double, tau: Double) {
    var t = t0
    while (t < t0+tau) {
      // We do the setting in two steps so as not to perturb the
      // t values when setting t+1. So first calculate all the
      // t+1 values
      val tmp: Array[T] = Array.fill(vars.size)(rig.zero)
      for (i <- 0 until vars.size)
        tmp(i) = funcs(i)()
      // ... and *then* set the actual variables.
      for (i <- 0 until vars.size)
        vars(i) := tmp(i)
      t += stepSize
    }
  }
}
