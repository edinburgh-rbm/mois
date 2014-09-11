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
  // import all of scala/java Math
  import scala.math

  @inline final def sin(x: Double) = math.sin(x)
  @inline final def cos(x: Double) = math.cos(x)
  @inline final def cosh(x: Double) = math.cosh(x)
  @inline final def tan(x: Double) = math.tan(x)
  @inline final def asin(x: Double) = math.asin(x)
  @inline final def acos(x: Double) = math.acos(x)
  @inline final def atan(x: Double) = math.atan(x)
  @inline final def atan2(x: Double, y: Double) = math.atan2(x, y)
  @inline final def hypot(x: Double, y: Double) = math.hypot(x, y)

  @inline final def degrees(x: Double) = math.toDegrees(x)
  @inline final def radians(x: Double) = math.toRadians(x)

  @inline final def abs(x: Double) = math.abs(x)
  @inline final def ceil(x: Double) = math.ceil(x)
  @inline final def floor(x: Double) = math.floor(x)
  @inline final def min(x: Double, y: Double) = math.min(x, y)
  @inline final def max(x: Double, y: Double) = math.max(x, y)

  @inline final def exp(x: Double) = math.exp(x)
  @inline final def pow(x: Double, y: Double) = math.pow(x, y)
  @inline final def sqrt(x: Double) = math.sqrt(x)
  @inline final def ln(x: Double) = math.log(x)
  @inline final def log(x: Double): Double = log(x, 10)
  @inline final def log(x: Double, base: Double): Double = math.log(x)/math.log(base)
}
