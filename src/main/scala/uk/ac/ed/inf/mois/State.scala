/*
 *  MOIS: State interface
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
import scala.language.existentials
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.reflect.ClassTag

import spire.algebra.Rig
import spire.math.{Complex, Natural, Rational, Real}
import spire.implicits._

/** State is just a list of variables, one array for each type
  * and a corresponding list of metadata that is used to build
  * an index so the state can be conveniently accessed.
  */
trait State {
  /** return a variable handle for the given metadata.
    * this is a relatively expensive operation because it may
    * allocate objects and should be avoided where possible
    */
  def getVar[T](m: VarMeta)(implicit rig: Rig[T]): Var[T]
  /** return the list of metadata for the given type */
  def getMeta[T](implicit rig: Rig[T]): Seq[VarMeta]
  /** return the types that have been registered in this state */
  def getTypes: Seq[Rig[_]]
  /** return all of the variables of a given type */
  def get[T](implicit rig: Rig[T]): Array[T]

  /** does this variable exist in this state? */
  def hasVar(m: VarMeta): Boolean
  /** do any variables of the given type (Rig) exist in this state? */
  def hasType(rig: Rig[_]): Boolean
  /** does the variable exist in this state with the given type */
  def hasVarType[T](m: VarMeta)(implicit rig: Rig[T]): Boolean

  /** copy the state including any backing storage */
  def deepCopy: State
  /** write all variables of the given type in the intersection of
    * this and other from this to other */
  def copyTo[T](other: State)(implicit rig: Rig[T]): Unit
  /** write all variables of the given type in the intersection of
    * other and this from other to this */
  def copyFrom[T](other: State)(implicit rig: Rig[T]): Unit
  /** write all variables in the intersection of this and other
    * from this to other (seeAlso [[StateSyntax.<<<]]*/
  def copyToAll(other: State) {
    val types = getTypes
    for (rig <- other.getTypes if types contains rig)
      copyTo(other)(rig)
  }
  /** write all variables in the intersection of other and this
    * from other to this (seeAlso [[StateSyntax.<<<]]) */
  def copyFromAll(other: State) {
    val types = other.getTypes
    for (rig <- getTypes if types contains rig)
      copyFrom(other)(rig)
  }

  /** bulk update the data backing store for the given type.
    * this assumes that data and the backing store are of the
    * same size, or otherwise compatible in an implementation
    * specific way */
  def update[T](data: Array[T])(implicit rig: Rig[T]): Unit
}

/** StateBuilder is used, unsurprisingly, to build a [[State]]. It
  * provides facilities for adding variables to its own mutable data
  * structures, merging information from other StateBuilders, and so
  * forth, and when this is all done, creating a compact and useable
  * [[State]].
  *
  * It is very important to call [[StateBuilder.initState]]
  * once and only once before using any state.
  *
  * Example:
  * {{{
  * class Foo extends StateBuilder {
  *   val x = Double("x")
  *   val y = Boolean("y")
  *   val z = BigInt("z")
  *
  *   val state = buildState
  *   initState(state)
  *
  *   // now x, y, z can be used as desired
  * }
  * }}}
  */
trait StateBuilder {
  /** Construct a [[State]] */
  def buildState: State
  /** Initialise the [[State]] */
  def initState(s: State)
  /** Get the [[State]] */
  def getState: State

  // intermediate datastructures to hold a partially built state
  protected[mois] class Bag[T: ClassTag](implicit rig: Rig[T]) {
    self =>
    val metas = mutable.ArrayBuffer.empty[VarMeta]
    def add(meta: VarMeta) = metas += meta
    def values: Array[T] = Array.fill[T](metas.size)(rig.zero)
    def copy: Bag[T] = new Bag[T] {
      override val metas = self.metas.clone
    }
  }
  protected[mois] val bags = mutable.Map.empty[Rig[_], Bag[_]]
  protected[mois] val allmeta = mutable.Set.empty[VarMeta]
  protected[mois] val _defaults = mutable.ArrayBuffer.empty[(State => Unit)]
  protected[mois] implicit class Default[T](v: Var[T])(implicit rig: Rig[T]) {
    def default(x: T) = {
      _defaults += (state => state.getVar[T](v.meta).update(x))
      v
    }
  }
  def setDefaults {
    for (f <- _defaults) f(getState)
  }

  /** Merge a partially built [[State]] with this one */
  def merge(other: StateBuilder) {
    for ((rig, bag) <- other.bags) {
      if (!(bags contains rig)) {
        bags(rig) = bag.copy.asInstanceOf[Bag[_]]
      } else {
        for(m <- bag.metas) {
          if (bags(rig).metas contains m) {
            val mine = bags(rig).metas(bags(rig).metas.indexOf(m))
            mine merge(m)
          } else {
            bags(rig) add m
          }
        }
      }
    }
    _defaults prependAll(other._defaults)
  }

  protected def createVar[T : Rig : ClassTag](meta: VarMeta): Var[T]

  /** Add a variable to the under construction proto[[State]]
    *
    * @param meta is the metadata used to index the state
    * @return a handle that can be used to access this variable later.
    */
  def addVar[T: ClassTag](ident: String)(implicit rig: Rig[T]): Var[T] = {
    val meta = new VarMeta(ident, rig)
    // check that we do not already know this variable
    if (!(allmeta contains meta)) {
      allmeta += meta
      // we have never seen any variable of this type
      if (!(bags contains rig))
        bags(rig) = new Bag[T]
      bags(rig) add meta
    } else {
      // we already have a variable called this, and are just being
      // asked for another index for it. make sure index is the right
      // type
      require(bags(rig).metas contains meta, "requested index for wrong type")
    }

    createVar[T](meta)
  }

  object Int {
    def apply(ident: String) = addVar[Int](ident)
  }
  object Byte {
    def apply(ident: String) = addVar[Byte](ident)
  }
  object Long {
    def apply(ident: String) = addVar[Long](ident)
  }
  object Real {
    def apply(ident: String) = addVar[Real](ident)
  }
  object Float {
    def apply(ident: String) = addVar[Float](ident)
  }
  object Short {
    def apply(ident: String) = addVar[Short](ident)
  }
  object BigInt {
    def apply(ident: String) = addVar[BigInt](ident)
  }
  object Double {
    def apply(ident: String) = addVar[Double](ident)
  }
  object Boolean {
    def apply(ident: String) = addVar[Boolean](ident)
  }
  object Complex {
    def apply(ident: String) = addVar[Complex[Double]](ident)
  }
  object Natural {
    def apply(ident: String) = addVar[Natural](ident)
  }
  object Rational {
    def apply(ident: String) = addVar[Rational](ident)
  }
  object BigDecimal {
    def apply(ident: String) = addVar[BigDecimal](ident)
  }
}
