/*
 *  MOIS: Kinetic Reaction Network
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
package uk.ac.ed.inf.mois.reaction

import spire.math.ConvertableFrom
import spire.algebra.Field
import uk.ac.ed.inf.mois.Var
import uk.ac.ed.inf.mois.math.Multiset

private[mois] trait BaseKineticReactionNetwork[R, D] extends ReactionNetwork[R] {
  trait KineticReaction extends BaseReaction {
    def rate: D
    override def stringPrefix = "KineticReaction"
    override def toString = stringPrefix +
      "(" + lhs + ", " + rhs + ", " + rate + ")"
  }

  trait KineticReactionFactory {
    def apply(lhs: Multiset[Species], rhs: Multiset[Species], k: () => D)
      (implicit field: Field[D]): KineticReaction
  }
}

trait MassActionReactionNetwork[R, D] extends BaseKineticReactionNetwork[R, D] {
  def count(m: Multiset[Species]): D

  class MassActionReaction(
    val lhs: Multiset[Species], 
    val rhs: Multiset[Species],
    val k: () => D
  )(implicit field: Field[D]) extends KineticReaction {
    override def stringPrefix = "MassActionReaction"
    def rate = field.times(count(lhs), k())
  }

  val MassActionReaction: KineticReactionFactory =
    new KineticReactionFactory {
      def apply(lhs: Multiset[Species], rhs: Multiset[Species], k: () => D)(implicit field: Field[D]) =
        new MassActionReaction(lhs, rhs, k)
    }
}

trait RateLawReactionNetwork[R, D] extends BaseKineticReactionNetwork[R, D] {
  class RateLawReaction(
    val lhs: Multiset[Species], val rhs: Multiset[Species], val k: () => D)
      extends KineticReaction {
    override def stringPrefix = "RateLawReaction"
    def rate = k()
  }

  val RateLawReaction: KineticReactionFactory =
    new KineticReactionFactory {
      def apply(lhs: Multiset[Species], rhs: Multiset[Species], k: () => D)(implicit field: Field[D]) =
        new RateLawReaction(lhs, rhs, k)
    }
}

/** A trait for reaction networks that have kinetic rates. */
trait KineticReactionNetwork[R, D]
    extends MassActionReactionNetwork[R, D]
    with RateLawReactionNetwork[R, D] {

  type Reaction <: UnratedReaction

  trait UnratedReaction extends BaseReaction {
    def at(k: => D)(implicit field: Field[D]) = MassActionReaction(lhs, rhs, () => k)
    def `at!`(k: => D)(implicit field: Field[D]) = RateLawReaction(lhs, rhs, () => k)
  }

  implicit class DoubleArithmetic(v: Var[R])(
    implicit d: ConvertableFrom[R]
  ){
    def *(u: Var[R]): Double = d.toDouble(v.value) * d.toDouble(u.value)
    def *(x: R): Double = d.toDouble(v.value) * d.toDouble(x)
    def *(x: Double): Double = d.toDouble(v.value) * x
  }
}
