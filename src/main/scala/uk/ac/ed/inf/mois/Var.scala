/*
 *  MOIS: Variable Types
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

import scala.language.existentials
import scala.language.implicitConversions
import spire.algebra.{Rig, Ring, Field}

/** This class is to abstract away the details of uniquely identifying a
  * state variable.
  */
case class VarMeta(identifier: String, rig: Rig[T] forSome { type T })
    extends Ordered[VarMeta] with Annotation {
  def compare(that: VarMeta) = this.identifier compare that.identifier
  override def toString = identifier
}

trait Var[T] {
  val meta: VarMeta
  /** Add an [[Annotation]] onto the [[VarMeta]] */
  val annotate = meta.annotate _

  def value: T
  def update(value: T): Unit
  @inline final def := (value: T) = { update(value); this }
}

object VarMeta {
  implicit def stringToMeta[T](s: String)(implicit rig: Rig[T])
    = VarMeta(s, rig)
}
