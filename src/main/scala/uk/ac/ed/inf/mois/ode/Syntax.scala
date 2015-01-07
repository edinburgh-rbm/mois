/*
 *  MOIS: Ordinary Differential Equations Syntax
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
package uk.ac.ed.inf.mois.ode

import language.implicitConversions
import scala.collection.mutable
import spire.algebra.{Field, NRoot, Ring}
import uk.ac.ed.inf.mois.Var

trait ODEBase[T, D] {
  type DerivativeValue = D
  type Derivative = () => DerivativeValue
  /** Functions defining the derivatives of the variables in `vars`.
    * The two arrays are indexed equally.
    */
  protected[mois] val funs = mutable.ArrayBuffer.empty[Derivative]

  /** An array with all `Var`s for which to integrate. */
  protected[mois] val vars = mutable.ArrayBuffer.empty[Var[T]]

  protected[mois] def vToD(v: Var[T]): D

  protected[mois] val _rg: Ring[D]
  protected[mois] val _nr: NRoot[D]
  protected[mois] val _fd: Field[D]
  protected[mois] def _fromInt(i: Int): D
}

trait ODESyntax[T, D] extends ODEBase[T, D] {
  private def addF(v: Var[T], f: Derivative) {
    vars += v
    funs += f
  }

  def derivativeCount = vars.size
  def clearDerivatives = {
    funs.clear
    vars.clear
  }

  /** A class to define derivatives of `Var`s. */
  protected class AddODE(val vs: Seq[Var[T]]) {

    /** Adds an ODE definition to the process. */
    def := (fs: Derivative*): Unit = {
      require(fs.size == vs.size,
        "lhs and rhs of ODE system must have same size")
      (vs zip fs) map { case (v, f) => addF(v, f)}
    }
  }
  implicit def bynameToFun(f: => D) = () => f
  implicit def varToFun(f: Var[T]) = () => f.value

  /** Adds an ODE definition to the current `ODE`. */
  protected def d(vs: Var[T]*) = new AddODE(vs) {
    def / (d: dt.type) = new AddODE(vs)
  }

  /** Object `dt` is used for writing ODEs with syntax: d(v1)/dt = ... */
  object dt
}
