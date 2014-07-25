package uk.ac.ed.inf.mois

import scala.language.implicitConversions

/** A trait for reaction networks that have catalysis. */
trait CatalyticReactionNetwork extends ReactionNetwork {

  type Reaction <: CatalysableReaction

  abstract class EnzymeMechanism

  trait CatalysableReaction extends BaseReaction {
    def catalysedBy(catalyser: Specie) =
      new ReactionWithCatalyser(lhs, rhs, catalyser)
  }

  class ReactionWithCatalyser(
    lhs: Multiset, rhs: Multiset, catalyser: Specie) {
    def using[M <: EnzymeMechanism](mechanism: M) =
      CatalysedReaction[M](lhs, rhs, catalyser, mechanism)
  }

  case class CatalysedReaction[Mechanism <: EnzymeMechanism](
    lhs: Multiset, rhs: Multiset, catalyser: Specie,
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
    def expand(lhs: Multiset, rhs: Multiset, catalyser: Specie)
        : Seq[KineticReaction]
  }

  def enzymeComplex(enzyme: Specie, substrates: Specie*) = {
    val name = (s: Specie) => s.meta.identifier
    Specie(name(enzyme) + "-" + substrates.map(name).mkString("-"))
  }

  /** Michaelis-Menten mechanism: E + S <->[k1,k2] ES ->[k3] E + P */
  case class MM(k1: Double, k2: Double, k3: Double)
      extends KineticMechanism {
    def expand(lhs: Multiset, rhs: Multiset, catalyser: Specie) = {
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
  case class QSS(vmax: Double, km: Double) extends KineticMechanism {
    def expand(lhs: Multiset, rhs: Multiset, catalyser: Specie) =
      List(lhs -> rhs `at!` vmax * s / (km + s))
  }

  implicit def catalyticToKinetic[M <: KineticMechanism](
    r: CatalysedReaction[M]) =
    r.mechanism.expand(r.lhs, r.rhs, r.catalyser)
}

