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
import scala.math.max
import spire.algebra.{Rig, Ring, Field, Eq}

case class VarFlags(
  val param: Boolean = false,
  val dimension: Boolean = false,
  val slices: Int = 1
)

/** This class is to abstract away the details of uniquely identifying a
  * state variable.
  */
case class VarMeta(identifier: String, rig: Rig[T] forSome { type T })
    extends Ordered[VarMeta] with Annotation {
  def compare(that: VarMeta) = this.identifier compare that.identifier
  override def toString = identifier
  def merge(other: VarMeta) {
    // TODO: Sanity Check things like units and types
    annotations ++= other.annotations
    flags.param = flags.param || other.flags.param
    flags.dimension = flags.dimension || other.flags.dimension
    flags.slices = max(flags.slices, other.flags.slices)
  }
  protected[mois] object flags {
    var param: Boolean = false
    var dimension: Boolean = false
    var slices: Int = 1
  }
}

object VarMeta {
  implicit def stringToMeta[T](s: String)(implicit rig: Rig[T])
    = VarMeta(s, rig)
}

final class VarMetaEq extends Eq[VarMeta] {
  def eqv(x: VarMeta, y: VarMeta) = {
    import spire.std.string._
    Eq[String].eqv(x.identifier, y.identifier)
  }
}

trait VarMetaInstances {
  implicit val VarMetaEq = new VarMetaEq
}

object var_meta extends VarMetaInstances

trait Var[T] extends Constraints[T] with Bounds[T] {
  val meta: VarMeta
  /** Add an [[Annotation]] onto the [[VarMeta]] */
  val annotate = meta.annotate _

  /** rig is a proxy for the type of this variable */
  val rig = meta.rig.asInstanceOf[Rig[T]]

  def value: T
  def update(value: T): Unit
  @inline final def := (value: T) = { update(doClamp(value)); this }
  override def equals(other: Any) = {
    import var_meta._
    other match {
      case that: Var[T] => Eq[VarMeta].eqv(meta, that.meta)
      case _ => false
    }
  }
  override def hashCode = meta.identifier.hashCode

  private val _fromString = coerceString(rig)
  /** brittle conversion from string. only converts, does not set */ 
  def fromString(s: String): T = _fromString(s)
  /** brittle update from string, uses [[fromString]] */
  def updateFromString(s: String) { update(_fromString(s)) }

  def isParam = meta.flags.param
  def isDimension = meta.flags.dimension
}

final class VarEqByMeta[T] extends Eq[Var[T]] {
  def eqv(x: Var[T], y: Var[T]) =  {
    import var_meta._
    Eq[VarMeta].eqv(x.meta, y.meta)
  }
}

final class VarEqByValue[T: Eq] extends Eq[Var[T]] {
  def eqv(x: Var[T], y: Var[T]) = Eq[T].eqv(x.value, y.value)
}

final class VarEqByBoth[T: Eq] extends Eq[Var[T]] {
  def eqv(x: Var[T], y: Var[T]) = {
    import var_meta._
    Eq[T].eqv(x.value, y.value) && Eq[VarMeta].eqv(x.meta, y.meta)
  }
}

trait VarByMetaInstances {
  implicit def VarEq[T] = new VarEqByMeta[T]
}

trait VarByValueInstances {
  implicit def VarEq[T: Eq] = new VarEqByValue[T]
}

trait VarByBothInstances {
  implicit def VarEq[T: Eq] = new VarEqByBoth[T]
}

object var_bymeta extends VarByMetaInstances
object var_byvalue extends VarByValueInstances
object var_byboth extends VarByBothInstances
