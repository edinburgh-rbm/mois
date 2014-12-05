/*
 *  MOIS: String Conversion Coercion
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

import spire.algebra.Rig
import spire.implicits._

private[mois] object coerceString {
  def apply[T](rig: Rig[T]): String => T = {
    import scala.Predef.augmentString
    if (rig == Rig[Int]) {
      (toInt _).asInstanceOf[String => T]
    } else if (rig == Rig[Byte]) {
      (toByte _).asInstanceOf[String => T]
    } else if (rig == Rig[Long]) {
      (toLong _).asInstanceOf[String => T]
    } else if (rig == Rig[Short]) {
      (toShort _).asInstanceOf[String => T]
    } else if (rig == Rig[Float]) {
      (toFloat _).asInstanceOf[String => T]
    } else if (rig == Rig[Double]) {
      (toDouble _).asInstanceOf[String => T]
    } else {
      def zero(s: String) = {
        // FIXME: proper warning or assert something here
        println(s"Warning: unknown string conversion in ${rig} for ${s}")
        rig.zero
      }
        (zero _).asInstanceOf[String => T]
    }
  }

  def toInt(s: String) = augmentString(s).toInt
  def toByte(s: String) = augmentString(s).toByte
  def toLong(s: String) = augmentString(s).toLong
  def toShort(s: String) = augmentString(s).toShort
  def toFloat(s: String) = augmentString(s).toFloat
  def toDouble(s: String) = augmentString(s).toDouble
}

private[mois] object typeExtrema {
  def apply[T](rig: Rig[T]): (T, T, T) = {
    if (rig == Rig[Int]) {
      (scala.Int.MinValue, scala.Int.MaxValue, scala.Int.MinValue)
        .asInstanceOf[(T, T, T)]
    } else if (rig == Rig[Byte]) {
      (scala.Byte.MinValue, scala.Byte.MaxValue, scala.Byte.MinValue)
        .asInstanceOf[(T, T, T)]
    } else if (rig == Rig[Long]) {
      (scala.Long.MinValue, scala.Long.MaxValue, scala.Long.MinValue)
        .asInstanceOf[(T, T, T)]
    } else if (rig == Rig[Short]) {
      (scala.Short.MinValue, scala.Short.MaxValue, scala.Short.MinValue)
        .asInstanceOf[(T, T, T)]
    } else if (rig == Rig[Float]) {
      (scala.Float.MinValue, scala.Float.MaxValue, scala.Float.NaN)
        .asInstanceOf[(T, T, T)]
    } else if (rig == Rig[Double]) {
      (scala.Double.MinValue, scala.Double.MaxValue, scala.Double.NaN)
        .asInstanceOf[(T, T, T)]
    } else {
      (rig.zero, rig.zero, rig.zero)
    }
  }
}
