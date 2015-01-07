/*
 *  MOIS: Catalytic Reaction Network
 *  Copyringht (C) 2014 University of Edinburgh School of Informatics
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
import scala.reflect.ClassTag
import spire.algebra.{Field, Ring}
import uk.ac.ed.inf.mois.math.Multiset

/** A trait for reaction networks that have catalysis. */
trait CatalyticReactionNetwork[T] extends ReactionNetwork[T] {

  type Reaction <: CatalysableReaction

  abstract class EnzymeMechanism

  trait CatalysableReaction extends BaseReaction {
    def catalysedBy(catalyser: Species) = new {
      def using[M <: EnzymeMechanism](mechanism: M) =
        CatalysedReaction[M](lhs, rhs, catalyser, mechanism)
    }
  }

  case class CatalysedReaction[Mechanism <: EnzymeMechanism](
    lhs: Multiset[Species], rhs: Multiset[Species], catalyser: Species,
    mechanism: Mechanism) extends BaseReaction
}


/** A trait for reaction networks that have catalysis. */
trait KineticCatalyticReactionNetwork[T, D]
    extends KineticReactionNetwork[T, D]
       with CatalyticReactionNetwork[T] {

  type Reaction <: UnratedReaction with CatalysableReaction

  def enzymeComplex(enzyme: Species, substrates: Species*)(
    implicit ring: Ring[T], ct: ClassTag[T]) =
    Species(enzyme.meta + "-" + substrates.map(_.meta).mkString("-"))

  /** Michaelis-Menten mechanism: E + S <->[k1,k2] ES ->[k3] E + P */
  case class MM(k1: D, k2: D, k3: D)
      extends EnzymeMechanism

  /** Quasi-steady-state approximation. */
  case class QSS(vmax: D, km: D)
      extends EnzymeMechanism

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
    bind1: D, unbind1: D,
    bind2: D, unbind2: D,
    fwdCat: D, bwdCat: D,
    bind3: D, unbind3: D,
    bind4: D, unbind4: D)
      extends EnzymeMechanism

  // TODO: By giving all the species involved in the reaction as
  // parameters to TCOrdered we are basically specifying the reaction
  // twice.  This could be solved by using an ordered map for
  // multisets and reverting TCOrdered to its first implementation.
  /** Ternary-complex mechanism where `a` is bound first, then `b`,
    * then `p` is released and finally `q` is released.  See
    * en.wikipedia.org/wiki/Enzyme_kinetics#Ternary-complex_mechanisms
    *
    * @param a first substrate and its binding and unbinding rate.
    * @param b second substrate and its binding and unbinding rate.
    * @param fwdCat kinetic rate for forward catalytic step.
    * @param bwdCat kinetic rate for backward catalytic step.
    * @param p first product and its binding and unbinding rate.
    * @param q second product and its binding and unbinding rate.
    */
  case class TCOrdered(
    a: (Species, (D, D)),
    b: (Species, (D, D)),
    fwdCat: D, bwdCat: D,
    p: (Species, (D, D)),
    q: (Species, (D, D)))
      extends EnzymeMechanism

  /** Ping-pong mechanism.  See
    * en.wikipedia.org/wiki/Enzyme_kinetics#Ping-pong_mechanisms
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
  case class PP(
    bind1: D, unbind1: D,
    fwd1: D, bwd1: D,
    bind3: D, unbind3: D,
    bind2: D, unbind2: D,
    fwd2: D, bwd2: D,
    bind4: D, unbind4: D)
      extends EnzymeMechanism

  def check(r: CatalysedReaction[_], size: Int, mech: String) = {
    require(r.lhs.multisize == size, "left-hand side of reaction " +
      r.lhs + " -> " + r.rhs + " must have exactly " + size +
      " substrate" + (if (size > 1) "s" else "") + " to use a " +
      mech + " mechanism.")
    require(r.rhs.multisize == size, "ringht-hand side of reaction " +
      r.lhs + " -> " + r.rhs + " must have exactly " + size +
      " product" + (if (size > 1) "s" else "") + " to use a " +
      mech + " mechanism.")
  }

  // -- Expand catalytic reactions into sets of kinetic reactions --

  implicit def mm(r: CatalysedReaction[MM])(implicit field: Field[D], ring: Ring[T], ct: ClassTag[T]) = {
    check(r, 1, "Michaelis-Menten (MM)")
    val (s, _) = r.lhs.head
    val (p, _) = r.rhs.head
    val e  = r.catalyser
    val es = enzymeComplex(e, s)
    import r.mechanism._
    List(e + s --> es at k1,
         es --> e + s at k2,
         es --> e + p at k3)
  }

  implicit def qss(r: CatalysedReaction[QSS])(implicit field: Field[D], ring: Ring[D]) = {
    import r.mechanism.{vmax, km}
    List(r.lhs --> r.rhs `at!`
      field.div(
        field.times(vmax, count(r.lhs)), 
        ring.plus(km, count(r.lhs))
      )
    )
  }

  implicit def tcrandom(r: CatalysedReaction[TCRandom])(
    implicit field: Field[D], ring: Ring[T], ct: ClassTag[T]) = {
    check(r, 2, "ternary-complex (TCRandom)")
    val Seq(s1, s2) = r.lhs.multiseq
    val Seq(p1, p2) = r.rhs.multiseq
    val e = r.catalyser
    val es1 = enzymeComplex(e, s1)
    val es2 = enzymeComplex(e, s2)
    val es12 = enzymeComplex(es1, s2)
    val ep1 = enzymeComplex(e, p1)
    val ep2 = enzymeComplex(e, p2)
    val ep12 = enzymeComplex(ep1, p2)
    import r.mechanism._
    List(e + s1 --> es1 at bind1,
         e + s2 --> es2 at bind2,
         es1 --> e + s1 at unbind1,
         es2 --> e + s2 at unbind2,
         es2 + s1 --> es12 at bind1,
         es1 + s2 --> es12 at bind2,
         es12 --> es2 + s1 at unbind1,
         es12 --> es1 + s2 at unbind2,
         es12 --> ep12 at fwdCat,
         ep12 --> es12 at bwdCat,
         ep12 --> ep2 + p1 at unbind3,
         ep12 --> ep1 + p2 at unbind4,
         ep2 + p1 --> ep12 at bind3,
         ep1 + p2 --> ep12 at bind4,
         ep1 --> e + p1 at unbind3,
         ep2 --> e + p2 at unbind4,
         e + p1 --> ep1 at bind3,
         e + p2 --> ep2 at bind4)
  }

  implicit def tcordered(r: CatalysedReaction[TCOrdered])(
    implicit field: Field[D], ring: Ring[T], ct: ClassTag[T]) = {
    check(r, 2, "ternary-complex (TCOrdered)")
    import r.mechanism._
    val (s1, (bind1, unbind1)) = a
    val (s2, (bind2, unbind2)) = b
    val (p1, (bind3, unbind3)) = p
    val (p2, (bind4, unbind4)) = q
    require((r.lhs contains s1) && (r.lhs contains s2),
      "left-hand side of reaction " + r.lhs + " -> " + r.rhs +
      " doesn't contains species " + s1 + " or " + s2)
    require((r.rhs contains p1) && (r.rhs contains p2),
      "ringht-hand side of reaction " + r.lhs + " -> " + r.rhs +
      " doesn't contains species " + p1 + " or " + p2)
    val e = r.catalyser
    val es1  = enzymeComplex(e, s1)
    val es12 = enzymeComplex(es1, s2)
    val ep2  = enzymeComplex(e, p2)
    val ep12 = enzymeComplex(ep2, p1)
    List(e + s1 --> es1 at bind1,
         es1 --> e + s1 at unbind1,
         es1 + s2 --> es12 at bind2,
         es12 --> es1 + s2 at unbind2,
         es12 --> ep12 at fwdCat,
         ep12 --> es12 at bwdCat,
         ep12 --> ep2 + p1 at unbind3,
         ep2 + p1 --> ep12 at bind3,
         ep2 --> e + p2 at unbind4,
         e + p2 --> ep2 at bind4)
  }

  implicit def pp(r: CatalysedReaction[PP])(
    implicit field: Field[D], ring: Ring[T], ct: ClassTag[T]) = {
    check(r, 2, "ping-pong (PP)")
    val Seq(s1, s2) = r.lhs.multiseq
    val Seq(p1, p2) = r.rhs.multiseq
    val e1 = r.catalyser
    val e2 = Species(e1.meta + "'")
    val e1s1 = enzymeComplex(e1, s1)
    val e2p1 = enzymeComplex(e2, p1)
    val e2s2 = enzymeComplex(e2, s2)
    val e1p2 = enzymeComplex(e1, p2)
    import r.mechanism._
    List(e1 + s1 --> e1s1 at bind1,
         e1s1 --> e1 + s1 at unbind1,
         e1s1 --> e2p1 at fwd1,
         e2p1 --> e1s1 at bwd1,
         e2p1 --> e2 + p1 at unbind3,
         e2 + p1 --> e2p1 at bind3,
         e2 + s2 --> e2s2 at bind2,
         e2s2 --> e2 + s2 at unbind2,
         e2s2 --> e1p2 at fwd2,
         e1p2 --> e2s2 at bwd2,
         e1p2 --> e1 + p2 at unbind4,
         e1 + p2 --> e1p2 at bind4)
  }
}
