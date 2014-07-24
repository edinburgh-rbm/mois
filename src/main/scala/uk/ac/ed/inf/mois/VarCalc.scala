/*
 *  MOIS: VarCalc Process Helper Trait
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

trait VarCalc extends BaseProcess {
  type Func = () => Double

  private val vars = mutable.ArrayBuffer.empty[DoubleVar]
  private val funcs = mutable.ArrayBuffer.empty[Func]

  protected class Calc(val v: DoubleVar) {
    def := (e: => Double): Unit = {
      vars += v
      funcs += (() => e)
    }
  }

  @inline final def calc(v: DoubleVar) = new Calc(v)

  protected class CalcVars extends StepHandler {
    def init(t: Double, proc: BaseProcess) {
      handleStep(t, proc)
    }
    def handleStep(t: Double, proc: BaseProcess) {
      for ((v, f) <- vars zip funcs) {
	v := f()
      }
    }
  }

  addStepHandler(new CalcVars)
}
