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

import spire.algebra.{Rig, Ring, Field}

abstract class ImmutableVar[T] extends Var[T] {
  def update(value: T) {}
}

class RigVarIsRig[T: Rig] extends Rig[Var[T]] {
  def zero = new ImmutableVar[T] {
    val meta = VarMeta("", Rig[T])
    def value = Rig[T].zero
  }
  def one = new ImmutableVar[T] {
    val meta = VarMeta("", Rig[T])
    def value = Rig[T].one
  }
  def plus(x: Var[T], y: Var[T]) = new ImmutableVar[T] {
    val meta = VarMeta("", Rig[T])
    def value = Rig[T].plus(x.value, y.value)
  }
  def times(x: Var[T], y: Var[T]) = new ImmutableVar[T] {
    val meta = VarMeta("", Rig[T])
    def value = Rig[T].times(x.value, y.value)
  }
}

class RingVarIsRing[T: Ring] extends RigVarIsRig[T] with Ring[Var[T]] {
  def negate(x: Var[T]) = new ImmutableVar[T] {
    val meta = VarMeta("", Ring[T])
    def value =  Ring[T].negate(x.value)
  }
}

class FieldVarIsField[T: Field] extends RingVarIsRing[T] with Field[Var[T]] {
  def gcd(a: Var[T], b: Var[T]) = new ImmutableVar[T] {
    val meta = VarMeta("", Field[T])
    def value = Field[T].gcd(a.value, b.value)
  }
  def mod(a: Var[T], b: Var[T]) = new ImmutableVar[T] {
    val meta = VarMeta("", Field[T])
    def value = Field[T].mod(a.value, b.value)
  }
  def quot(a: Var[T], b: Var[T]) = new ImmutableVar[T] {
    val meta = VarMeta("", Field[T])
    def value = Field[T].quot(a.value, b.value)
  }
  def div(a: Var[T], b: Var[T]) = new ImmutableVar[T] {
    val meta = VarMeta("", Field[T])
    def value = Field[T].div(a.value, b.value)
  }
}
