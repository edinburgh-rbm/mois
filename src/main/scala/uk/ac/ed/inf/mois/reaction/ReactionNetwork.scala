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
  type Species <: BaseSpecies
  type Reaction <: BaseReaction

  val species = mutable.ArrayBuffer.empty[Species]

  // -- Species --
  abstract class BaseSpecies(val v: Var[T])(implicit ring: Ring[T]) {
    this: Species =>

    val meta = v.meta

    @inline final def value = v.value

    type R >: this.type <: Species

    // -- Multiset creation methods --
    def + (p: Multiset[Species]) = p + this
    def + (s: Species) = Multiset(this) + s
    def * (m: Int) = Multiset(this -> m)

    // -- Reaction creation methods --
    def -> (p: Multiset[Species]) = Reaction(Multiset(this), p)
    def -> (s: Species) = Reaction(Multiset(this), Multiset(s))
    def -> () = Reaction(Multiset(this), Multiset())

    // -- Assignment methods
    def := (x: T) = v := x
    // -- Arithmetic methods
    def += (x: T) = v := ring.plus(v.value, x)
    def -= (x: T) = v := ring.minus(v.value, x)

    def stringPrefix = "Species"
  }

  implicit class MultisetMaker(n: Int) {
    def * (s: Species) = Multiset(s -> n)
    def * (p: Multiset[Species]) = p * n
    def apply(s: Species) = Multiset(s -> n)
    def apply(p: Multiset[Species]) = p * n
  }

  // -- Reactions --

  abstract class SimpleReaction {

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

  abstract class BaseReaction extends SimpleReaction {

    // -- Append species to the right-hand side --
    def + (m: Multiset[Species]) = Reaction(lhs, rhs + m)
    def + (s: Species) = Reaction(lhs, rhs + s)
    def * (n: Int) = Reaction(lhs, rhs * n)
  }

  implicit class ReactionMaker(u: Unit) {
    def -> (m: Multiset[Species]) = Reaction(Multiset.empty, m)
    def -> (s: Species) = Reaction(Multiset.empty, Multiset(s))
  }

  implicit class MultisetReactionMaker(l: Multiset[Species]) {
    def -> (m: Multiset[Species]) = Reaction(l, m)
    def -> (s: Species) = Reaction(l, Multiset(s))
    def -> () = Reaction(l, Multiset())
  }

  // -- Factories --

  abstract class SpeciesFactory {
    def apply(ident: String)(implicit ring: Ring[T], ct: ClassTag[T]): Species
  }

  val Species: SpeciesFactory

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
}
