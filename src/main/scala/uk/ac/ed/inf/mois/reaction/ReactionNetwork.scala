/*
 *  MOIS: Reaction Network
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

import scala.language.implicitConversions
import scala.collection.mutable
import scala.reflect.ClassTag
import spire.algebra.{Rig, Ring}
import uk.ac.ed.inf.mois.math.Multiset
import uk.ac.ed.inf.mois.{Process, Var, VarMeta}

/** A base trait for reaction networks.  These reaction network use
  * species as variables and let you define reactions using them.
  * This is the base class for DeterministicReactionNetwork,
  * StochasticReactionNetwork and FBA.
  */
trait ReactionNetwork[T] extends Process {

  override def stringPrefix = "ReactionNetwork"

  // All species in a ReactionNetwork must be copy numbers
  // (Int or Long) or concentrations (Float or Double).
  // This doesn't allow for hybrid models.
  type Base = T // for counting number of ocurrences of species
  type Species = Var[T]
  type Reaction <: BaseReaction

  val species = mutable.ArrayBuffer.empty[Species]


  // -- Reactions --

  abstract class BaseReaction {
    val lhs: Multiset[Species]
    val rhs: Multiset[Species]

    def stringPrefix = "Reaction"
    override def toString = stringPrefix + "(" + lhs + ", " + rhs + ")"
    override def equals(that: Any) = that match {
      case that: BaseReaction =>
        (this.lhs == that.lhs) && (this.rhs == that.rhs)
      case _ => false
    }
    def apply(s: Species) = rhs(s) - lhs(s)
  }

  // -- Factories --
  def Species(ident: String)(implicit rig: Rig[T], ct: ClassTag[T]): Species = {
    val s = addVar[T](ident)
    species += s
    s
  }

  abstract class ReactionFactory {
    def apply(lhs: Multiset[Species], rhs: Multiset[Species]): Reaction
  }
  val Reaction: ReactionFactory

  // -- Conversions --

  object MultisetConversion {
    implicit def multisetToDouble(m: Multiset[Species])(
      implicit num: Numeric[Base]): Base =
      (for ((s, n) <- m) yield
        num.times(num.fromInt(n), s.value)).sum(num)
  }

  // -- Species --
  final class SpeciesSyntax(val v: Species)(implicit ring: Ring[T]) {
    // -- Multiset creation methods --
    def + (p: Multiset[Species]) = p + v
    def + (s: Species) = Multiset(v) + s
    def * (m: Int) = Multiset(v -> m)

    // -- Reaction creation methods --
    def --> (p: Multiset[Species]) = Reaction(Multiset(v), p)
    def --> (s: Species) = Reaction(Multiset(v), Multiset(s))
    def --> () = Reaction(Multiset(v), Multiset())

    // -- Arithmetic methods
    def += (x: T) = v.update(ring.plus(v.value, x))
    def -= (x: T) = v.update(ring.minus(v.value, x))
  }

  final class MultisetMaker(n: Int) {
    def * (s: Species) = Multiset(s -> n)
    def * (p: Multiset[Species]) = p * n
    def apply(s: Species) = Multiset(s -> n)
    def apply(p: Multiset[Species]) = p * n
  }

  final class ReactionMaker(u: Unit) {
    def --> (m: Multiset[Species]) = Reaction(Multiset.empty, m)
    def --> (s: Species) = Reaction(Multiset.empty, Multiset(s))
  }

  final class MultisetReactionMaker(l: Multiset[Species]) {
    def --> (m: Multiset[Species]) = Reaction(l, m)
    def --> (s: Species) = Reaction(l, Multiset(s))
    def --> () = Reaction(l, Multiset())
  }

  final class ReactionSyntax(r: Reaction) {
    // -- Append species to the right-hand side --
    def + (m: Multiset[Species]) = Reaction(r.lhs, r.rhs + m)
    def + (s: Species) = Reaction(r.lhs, r.rhs + s)
    def * (n: Int) = Reaction(r.lhs, r.rhs * n)
  }

  implicit def SpeciesSyntax(s: Species)(implicit r: Ring[T]) = new SpeciesSyntax(s)(r)
  implicit def MultisetMaker(n: Int) = new MultisetMaker(n)
  implicit def ReactionMaker(u: Unit) = new ReactionMaker(u)
  implicit def MultisetReactionMaker(l: Multiset[Species]) = new MultisetReactionMaker(l)
  implicit def ReactionSyntax(r: Reaction) = new ReactionSyntax(r)
}
