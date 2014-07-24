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
trait KineticCatalyticReactionNetwork extends KineticReactionNetwork
    with CatalyticReactionNetwork {

  type Reaction <: UnratedReaction with CatalysableReaction

  /** A trait for `EnzymeMechanism`s that are used to expand a
    * catalytic reaction into a set of `KineticReaction`s.
    */
  trait KineticMechanism extends EnzymeMechanism {
    def expand(lhs: Multiset, rhs: Multiset, catalyser: Specie)
        : Seq[KineticReaction]
  }

  /** Michaelis-Menten mechanism: E + S <->[k1,k2] ES ->[k3] E + P */
  case class MM(k1: Double, k2: Double, k3: Double)
      extends KineticMechanism {
    def expand(lhs: Multiset, rhs: Multiset, catalyser: Specie) = {
      require(lhs.multisize == 1, "left-hand side of " +
        "Michaelis-Menten reaction must have only one substrate")
      require(rhs.multisize == 1, "right-hand side of " +
        "Michaelis-Menten reaction must have only one product")
      def name(s: Specie) = s.meta.identifier
      val (s, _) = lhs.head
      val (p, _) = rhs.head
      val e  = catalyser
      val es = Specie(name(e) + "-" + name(s))
      List(e + s -> es at k1,
           es -> e + s at k2,
           es -> e + p at k3)
    }
  }

  implicit def catalyticToKinetic[M <: KineticMechanism](
    r: CatalysedReaction[M]) =
    r.mechanism.expand(r.lhs, r.rhs, r.catalyser)
}

