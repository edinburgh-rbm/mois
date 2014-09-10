/*
 *  MOIS: Projection from State - State
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
import spire.algebra.Rig

case class Projection(from: State, to: State) {
  private val varMap = {
    val types = to.getTypes.filter(t => from.getTypes contains t)
    val mVarMap = mutable.Map.empty[Rig[_], Array[(Int, Int)]]
    for (t <- types) {
      val fmeta = from.getMeta(t)
      val tmeta = to.getMeta(t)
      mVarMap(t) = tmeta
        .filter(m => fmeta contains m)
        .foldLeft(Array.empty[(Int, Int)]){ (z, m) =>
        z ++ Seq((fmeta indexOf m, tmeta indexOf m))
      }
    }
    mVarMap.toMap
  }

  @inline private def copy_forward[T](rig: Rig[T], idxs: Array[(Int, Int)]) {
    val from_data: Array[T] = from.get(rig)
    val to_data: Array[T] = to.get(rig)
    for ((i, j) <- idxs) {
      to_data(j) = from_data(i)
    }
  }
  def forward = {
    for ((t, idxs) <- varMap) {
      copy_forward(t, idxs)
    }
  }

  @inline private def copy_reverse[T](rig: Rig[T], idxs: Array[(Int, Int)]) {
    val from_data: Array[T] = from.get(rig)
    val to_data: Array[T] = to.get(rig)
    for ((i, j) <- idxs) {
      from_data(i) = to_data(j)
    }
  }
  def reverse = {
    for ((t, idxs) <- varMap) {
      copy_reverse(t, idxs)
    }
  }
}
