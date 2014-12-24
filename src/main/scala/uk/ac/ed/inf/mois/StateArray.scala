/*
 *  MOIS: Array-backed State Implementation
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
import scala.reflect.ClassTag
import spire.algebra.Rig

case class ArrayBackedState(
  val meta: Map[Rig[_], Array[VarMeta]],
  val vars: Map[Rig[_], Array[_]]) extends State {

  def deepCopy: State = copy(
    vars = vars.map({ case (rig, a) => (rig, a.clone) }).toMap
  )

  def update[T](data: Array[T])(implicit rig: Rig[T]) =
    Array.copy(data, 0, vars(rig), 0, data.size)

  def getTypes = meta.keys.toSeq

  def getVar[T](m: VarMeta)(implicit rig: Rig[T]): Var[T] = {
    if (!(meta contains m.rig) || !(meta(m.rig) contains m))
      throw new NoSuchElementException(s"key not found $m")
    val i = new Index[T](m)
    i.setState(this)
    i
  }

  def getMeta[T](implicit rig: Rig[T]) =
    meta.getOrElse(rig, Array.empty[VarMeta]).toSeq

  def get[T](implicit rig: Rig[T]): Array[T] =
    vars(rig).asInstanceOf[Array[T]]

  def hasType(t: Rig[_]) = meta.keys.toSeq contains t
  def hasVar(m: VarMeta) = meta.values.foldLeft(
    Seq.empty[VarMeta]
  )((z, a) => z ++ a) contains m
  def hasVarType[T](m: VarMeta)(implicit rig: Rig[T]) = {
    val ms = meta.get(rig)
    ms.isDefined && (ms contains m)
  }

  // xxx inefficient!
  def copyTo[T](other: State)(implicit rig: Rig[T]) {
    val om = other.getTypes
    if (om contains rig) {
      val mine: Array[T] = this.get[T]
      val theirs: Array[T] = other.get[T]
      val mymeta = meta(rig)
      val theirmeta = other.getMeta(rig)
      for (i <- 0 until theirmeta.size) {
        val idx = mymeta indexOf theirmeta(i)
        if (idx >= 0) {
          theirs(i) = mine(idx)
        }
      }
    }
  }

  // xxx inefficient!
  def copyFrom[T](other: State)(implicit rig: Rig[T]) {
    val om = other.getTypes
    if (om contains rig) {
      val mine: Array[T] = this.get[T]
      val theirs: Array[T] = other.get[T]
      val mymeta = meta(rig)
      val theirmeta = other.getMeta(rig)
      for (i <- 0 until theirmeta.size) {
        val idx = mymeta indexOf theirmeta(i)
        if (idx >= 0) {
          mine(idx) = theirs(i)
        }
      }
    }
  }
}

/** An Index is used to access a specific [[ArrayBackedState]]
  * variable.
  *
  * It is instantiated lazily, and it is important to call
  * [[Index.setState]] before using it, otherwise null pointer
  * errors will result. It exposes the [[Var]] interface.
  *
  * @param meta is used to find the amht array and offset
  *             into the state
  */
class Index[@specialized T](val meta: VarMeta) extends Var[T] {

  private var _state: ArrayBackedState = null
  // RHZ: Why do we need to cache state?
  // WW: because it is a lazily built var
  lazy val state = _state
  private lazy val array: Array[T] = state.get(rig)
  private lazy val index = state.meta(rig) indexOf meta

  // RHZ: Why not just expose state_=?
  // WW: because the binding to the state object is supposed to be
  // immutable
  /** setState must be called once and only once before using
    * the index
    */
  def setState(s: ArrayBackedState) {
    require(_state == null, "setState may not be called more than once")
    _state = s
  }

  /** Explicitly retrieve the underlying value in the state */
  @inline def value = array(index)
  /** Update the underlying value in the state */
  @inline def update(value: T) { array(index) = value }

  override def toString = s"$meta = $value"
}

/** An implementation of [[StateBuilder]] that constructs
  * an [[ArrayBackedState]]
  */
trait ArrayBackedStateBuilder extends StateBuilder {
  type VarType[T] = Index[T]
  protected[mois] val indices = mutable.ArrayBuffer.empty[Index[_]]
  private var _state: ArrayBackedState = null

  def createVar[T : Rig : ClassTag](meta: VarMeta): Var[T] = {
    val i = new Index[T](meta)
    indices += i
    i
  }

  def buildState: State = ArrayBackedState(
      bags.map({ case (rig, bag) => (rig, bag.metas.toArray) }).toMap,
      bags.map({ case (rig, bag) => (rig, bag.values) }).toMap
  )

  def initState(s: State) {
    assume(s.isInstanceOf[ArrayBackedState], "state must be of ArrayBackedState type")
    val abs = s.asInstanceOf[ArrayBackedState]
    _state = abs
    indices map (_.setState(abs))
    setDefaults
  }

  def getState: State = _state
}
