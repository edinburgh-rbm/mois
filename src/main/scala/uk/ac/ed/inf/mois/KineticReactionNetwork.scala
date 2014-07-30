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
package uk.ac.ed.inf.mois

/** A trait for reaction networks that have kinetic rates. */
trait KineticReactionNetwork extends ReactionNetwork {

  type Reaction <: UnratedReaction

  trait UnratedReaction extends BaseReaction {
    def at(k: => Double) = MassActionReaction(lhs, rhs, () => k)
    def `at!`(k: => Double) = RateLawReaction(lhs, rhs, () => k)
  }

  trait KineticReaction extends BaseReaction {
    def rate: Double
    override def stringPrefix = "KineticReaction"
    override def toString = stringPrefix +
      "(" + lhs + ", " + rhs + ", " + rate + ")"
  }

  trait KineticReactionFactory {
    def apply(lhs: Multiset, rhs: Multiset, k: () => Double)
        : KineticReaction
  }

  def count(m: Multiset): Double

  class MassActionReaction(
    val lhs: Multiset, val rhs: Multiset, val k: () => Double)
      extends KineticReaction {
    override def stringPrefix = "MassActionReaction"
    def rate = count(lhs) * k()
  }

  val MassActionReaction: KineticReactionFactory =
    new KineticReactionFactory {
      def apply(lhs: Multiset, rhs: Multiset, k: () => Double) =
        new MassActionReaction(lhs, rhs, k)
    }

  class RateLawReaction(
    val lhs: Multiset, val rhs: Multiset, val k: () => Double)
      extends KineticReaction {
    override def stringPrefix = "RateLawReaction"
    def rate = k()
  }

  val RateLawReaction: KineticReactionFactory =
    new KineticReactionFactory {
      def apply(lhs: Multiset, rhs: Multiset, k: () => Double) =
        new RateLawReaction(lhs, rhs, k)
    }
}
