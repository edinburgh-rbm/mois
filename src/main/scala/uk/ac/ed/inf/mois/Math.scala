/*
 *  MOIS: Mathematics Helpers
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

/**
 * Math is a mix-in class that automatically makes the standard
 * mathematical functions available. It is intended for use in Process
 * definitions.
 */
trait Math {
  // import all of java Math
  import java.lang.{Math => jm}

  /** Syntax Sugar for Doubles
   *
   * XXX Do not use! Scala confusingly treats * as higher precendence
   * than ^ defined in this way! */
  implicit class DoubleSugar(x: Double) {
    @inline def ^(y: Double) = jm.pow(x, y)
  }
  /** Syntax Sugar for DoubleVars
   *
   * XXX Do not use! Scala confusingly treats * as higher precendence
   * than ^ defined in this way! */
  implicit class VarSugar(x: DoubleVar) {
    @inline def ^(y: Double) = jm.pow(x.value, y)
  }

  @inline final def sin(x: Double) = jm.sin(x)
  @inline final def cos(x: Double) = jm.cos(x)
  @inline final def cosh(x: Double) = jm.cosh(x)
  @inline final def tan(x: Double) = jm.tan(x)
  @inline final def asin(x: Double) = jm.asin(x)
  @inline final def acos(x: Double) = jm.acos(x)
  @inline final def atan(x: Double) = jm.atan(x)
  @inline final def atan2(x: Double, y: Double) = jm.atan2(x, y)
  @inline final def hypot(x: Double, y: Double) = jm.hypot(x, y)

  @inline final def degrees(x: Double) = jm.toDegrees(x)
  @inline final def radians(x: Double) = jm.toRadians(x)

  @inline final def abs(x: Double) = jm.abs(x)
  @inline final def ceil(x: Double) = jm.ceil(x)
  @inline final def floor(x: Double) = jm.floor(x)
  @inline final def min(x: Double, y: Double) = jm.min(x, y)
  @inline final def max(x: Double, y: Double) = jm.max(x, y)

  @inline final def exp(x: Double) = jm.exp(x)
  @inline final def pow(x: Double, y: Double) = jm.pow(x, y)
  @inline final def sqrt(x: Double) = jm.sqrt(x)
  @inline final def ln(x: Double) = jm.log(x)
  @inline final def log(x: Double): Double = log(x, 10)
  @inline final def log(x: Double, base: Double): Double = jm.log(x)/jm.log(base)
}
