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

  /** Ternary-complex mechanism in random order.  See
    * en.wikipedia.org/wiki/Enzyme_kinetics#Ternary-complex_mechanisms
    *
    * @param bind1 kinetic rate for binding of first substrate.
    * @param unbind1 kinetic rate for unbinding of first substrate.
    * @param bind2 kinetic rate for binding of second substrate.
    * @param unbind2 kinetic rate for unbinding of second substrate.
    * @param fwdCat kinetic rate for forward catalytic step.
    * @param bwdCat kinetic rate for backward catalytic step.
    * @param bind3 kinetic rate for binding of first product.
    * @param unbind3 kinetic rate for unbinding of first product.
    * @param bind4 kinetic rate for binding of second product.
    * @param unbind4 kinetic rate for unbinding of second product.
    */
  case class TCRandom(
    bind1: Double, unbind1: Double,
    bind2: Double, unbind2: Double,
    fwdCat: Double, bwdCat: Double,
    bind3: Double, unbind3: Double,
    bind4: Double, unbind4: Double)
      extends KineticMechanism {
    def expand(lhs: Multiset, rhs: Multiset, catalyser: Species) = {
      require(lhs.multisize == 2, "left-hand side of reaction " +
        lhs + " -> " + rhs + " must have exactly two substrates " +
        "to use the ternary-complex mechanism (TC).")
      require(rhs.multisize == 2, "right-hand side of reaction " +
        lhs + " -> " + rhs + " must have exactly two products " +
        "to use the ternary-complex mechanism (TC).")
      val Seq(s1, s2) = lhs.multiseq
      val Seq(p1, p2) = rhs.multiseq
      val e = catalyser
      val es1 = enzymeComplex(e, s1)
      val es2 = enzymeComplex(e, s2)
      val es12 = enzymeComplex(es1, s2)
      val ep1 = enzymeComplex(e, p1)
      val ep2 = enzymeComplex(e, p2)
      val ep12 = enzymeComplex(ep1, p2)
      List(e + s1 -> es1 at bind1,
           e + s2 -> es2 at bind2,
           es1 -> e + s1 at unbind1,
           es2 -> e + s2 at unbind2,
           es2 + s1 -> es12 at bind1,
           es1 + s2 -> es12 at bind2,
           es12 -> es2 + s1 at unbind1,
           es12 -> es1 + s2 at unbind2,
           es12 -> ep12 at fwdCat,
           ep12 -> es12 at bwdCat,
           ep12 -> ep2 + p1 at unbind3,
           ep12 -> ep1 + p2 at unbind4,
           ep2 + p1 -> ep12 at bind3,
           ep1 + p2 -> ep12 at bind4,
           ep1 -> e + p1 at unbind3,
           ep2 -> e + p2 at unbind4,
           e + p1 -> ep1 at bind3,
           e + p2 -> ep2 at bind4)
    }
  }

  /** Ternary-complex mechanism in random order.  See
    * en.wikipedia.org/wiki/Enzyme_kinetics#Ternary-complex_mechanisms
    *
    * @param bind1 kinetic rate for binding of first substrate.
    * @param unbind1 kinetic rate for unbinding of first substrate.
    * @param bind2 kinetic rate for binding of second substrate.
    * @param unbind2 kinetic rate for unbinding of second substrate.
    * @param fwdCat kinetic rate for forward catalytic step.
    * @param bwdCat kinetic rate for backward catalytic step.
    * @param bind3 kinetic rate for binding of first product.
    * @param unbind3 kinetic rate for unbinding of first product.
    * @param bind4 kinetic rate for binding of second product.
    * @param unbind4 kinetic rate for unbinding of second product.
    */
  case class TCOrdered(
    bind1: Double, unbind1: Double,
    bind2: Double, unbind2: Double,
    fwdCat: Double, bwdCat: Double,
    bind3: Double, unbind3: Double,
    bind4: Double, unbind4: Double)
      extends KineticMechanism {
    def expand(lhs: Multiset, rhs: Multiset, catalyser: Species) = {
      require(lhs.multisize == 2, "left-hand side of reaction " +
        lhs + " -> " + rhs + " must have exactly two substrates " +
        "to use the ternary-complex mechanism (TC).")
      require(rhs.multisize == 2, "right-hand side of reaction " +
        lhs + " -> " + rhs + " must have exactly two products " +
        "to use the ternary-complex mechanism (TC).")
      val Seq(s1, s2) = lhs.multiseq
      val Seq(p1, p2) = rhs.multiseq
      val e = catalyser
      val es1 = enzymeComplex(e, s1)
      val es12 = enzymeComplex(es1, s2)
      val ep2 = enzymeComplex(e, p2)
      val ep12 = enzymeComplex(ep2, p1)
      List(e + s1 -> es1 at bind1,
           es1 -> e + s1 at unbind1,
           es1 + s2 -> es12 at bind2,
           es12 -> es1 + s2 at unbind2,
           es12 -> ep12 at fwdCat,
           ep12 -> es12 at bwdCat,
           ep12 -> ep2 + p1 at unbind3,
           ep2 + p1 -> ep12 at bind3,
           ep2 -> e + p2 at unbind4,
           e + p2 -> ep2 at bind4)
    }
  }

  implicit def catalyticToKinetic[
    M <: KineticMechanism](r: CatalysedReaction[M]) =
    r.mechanism.expand(r.lhs, r.rhs, r.catalyser)
}
