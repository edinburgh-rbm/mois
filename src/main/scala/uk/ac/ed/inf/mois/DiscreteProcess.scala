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

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.collection.mutable

abstract class DiscreteProcess(val name: String)
    extends BaseProcess with VarConversions {

  override def stringPrefix = "DiscreteProcess"

  type Func = () => Double

  /** Configurable time step size. This is a discrete time process but
    * nevertheless might be integrated with a continuous time process.
    * This stepSize is the amount of continuous, real, time that a discrete
    * time step can be set to take. Defaults to 1.0, naturally.
    */
  val stepSize = 1.0

  private val vars = mutable.ArrayBuffer.empty[DoubleVar]
  private val funcs = mutable.ArrayBuffer.empty[Func]

  protected class Next(val v: DoubleVar) {
    def := (e: => Double): Unit = addNext(v, () => e)
  }

  protected def addNext(v: DoubleVar, f: Func) {
    vars += v.copy
    funcs += f
  }

  @inline final def next(v: DoubleVar) = new Next(v)
  @inline final def n(v: DoubleVar) = new Next(v)

  def step(t0: Double, tau: Double) {
    var t = t0
    while (t < t0+tau) {
      for ((v, f) <- vars zip funcs) {
	v := f()
      }
      for (v <- vars) {
	doubleVars(v) := v
      }
      t += stepSize
      for (sh <- stepHandlers)
        sh.handleStep(t, this)
    }
  }

  @inline override def apply(t: Double, tau: Double) = step(t, tau)
}

