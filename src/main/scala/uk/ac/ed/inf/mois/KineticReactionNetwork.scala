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
    override def toString =
      "KineticReaction(" + lhs + ", " + rhs + ", " + rate + ")"
  }

  trait KineticReactionFactory {
    def apply(lhs: Multiset, rhs: Multiset, k: () => Double)
        : KineticReaction
  }

  implicit val count: Multiset => Double

  class MassActionReaction(
    val lhs: Multiset, val rhs: Multiset, val k: () => Double)
      extends KineticReaction {
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
    def rate = k()
  }

  val RateLawReaction: KineticReactionFactory =
    new KineticReactionFactory {
      def apply(lhs: Multiset, rhs: Multiset, k: () => Double) =
        new RateLawReaction(lhs, rhs, k)
    }
}
