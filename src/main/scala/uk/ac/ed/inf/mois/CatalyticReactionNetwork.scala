/*
 *  MOIS: Catalytic Reaction Network
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

import scala.language.implicitConversions

/** A trait for reaction networks that have catalysis. */
trait CatalyticReactionNetwork extends ReactionNetwork {

  type Reaction <: CatalysableReaction

  abstract class EnzymeMechanism

  trait CatalysableReaction extends BaseReaction {
    def catalysedBy(catalyser: Species) =
      new ReactionWithCatalyser(lhs, rhs, catalyser)
  }

  class ReactionWithCatalyser(
    lhs: Multiset, rhs: Multiset, catalyser: Species) {
    def using[M <: EnzymeMechanism](mechanism: M) =
      CatalysedReaction[M](lhs, rhs, catalyser, mechanism)
  }

  case class CatalysedReaction[Mechanism <: EnzymeMechanism](
    lhs: Multiset, rhs: Multiset, catalyser: Species,
    mechanism: Mechanism) extends SimpleReaction
}


/** A trait for reaction networks that have catalysis. */
trait KineticCatalyticReactionNetwork
    extends KineticReactionNetwork
       with CatalyticReactionNetwork {

  type Reaction <: UnratedReaction with CatalysableReaction

  /** A trait for `EnzymeMechanism`s that are used to expand a
    * catalytic reaction into a set of `KineticReaction`s.
    */
  trait KineticMechanism extends EnzymeMechanism {
    def expand(lhs: Multiset, rhs: Multiset, catalyser: Species)
        : Seq[KineticReaction]
  }

  def enzymeComplex(enzyme: Species, substrates: Species*) = {
    val name = (s: Species) => s.meta.identifier
    Species(name(enzyme) + "-" + substrates.map(name).mkString("-"))
  }

  /** Michaelis-Menten mechanism: E + S <->[k1,k2] ES ->[k3] E + P */
  case class MM(k1: Double, k2: Double, k3: Double)
      extends KineticMechanism {
    def expand(lhs: Multiset, rhs: Multiset, catalyser: Species) = {
      require(lhs.multisize == 1, "left-hand side of reaction " +
        lhs + " -> " + rhs + " must have only one substrate to use " +
        "Michaelis-Menten (MM) mechanism.")
      require(rhs.multisize == 1, "right-hand side of reaction " +
        lhs + " -> " + rhs + " must have only one product to use " +
        "Michaelis-Menten (MM) mechanism.")
      val (s, _) = lhs.head
      val (p, _) = rhs.head
      val e  = catalyser
      val es = enzymeComplex(e, s)
      List(e + s -> es at k1,
           es -> e + s at k2,
           es -> e + p at k3)
    }
  }

  /** Quasi-steady-state approximation. */
  case class QSS(vmax: Double, km: Double)
      extends KineticMechanism {
    def expand(lhs: Multiset, rhs: Multiset, catalyser: Species) =
      List(lhs -> rhs `at!` vmax * count(lhs) / (km + count(lhs)))
  }

  implicit def catalyticToKinetic[M <: KineticMechanism](
    r: CatalysedReaction[M]) =
    r.mechanism.expand(r.lhs, r.rhs, r.catalyser)
}
