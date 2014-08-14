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

trait VarCalc extends Process {

  type Func = () => Unit

  private val funcs = mutable.ArrayBuffer.empty[Func]

  protected class Calc[T](val v: Index[T]) {
    def := (e: => T): Unit = {
      funcs += (() => v := e)
    }
  }

  @inline final def calc[T](v: Index[T]) = new Calc(v)

  protected class CalcVars extends StepHandler {
    def init(t: Double, proc: Process) {
      handleStep(t, proc)
    }
    def handleStep(t: Double, proc: Process) {
      for (f <- funcs) {
        f()
      }
    }
  }

  addStepHandler(new CalcVars)
}
