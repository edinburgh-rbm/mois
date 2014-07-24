package uk.ac.ed.inf.mois

import scala.math.{pow => dpow}

/** A trait for reaction networks that have kinetic rates. */
trait KineticReactionNetwork extends ReactionNetwork {

  type Reaction <: UnratedReaction

  trait UnratedReaction extends BaseReaction {
    def at(k: => Double) = new MassActionReaction(lhs, rhs, () => k)
    def `at!`(k: => Double) = new RateLawReaction(lhs, rhs, () => k)
  }

  trait KineticReaction extends BaseReaction {
    def rate: Double
    override def toString =
      "KineticReaction(" + lhs + ", " + rhs + ", " + rate + ")"
  }

  def ipow(base: Int, exp: Int, acc: Int): Int =
    if (exp == 1) acc else ipow(base, exp-1, acc*base)

  def pow(base: Base, exp: Int) = base match {
    case i: Int => ipow(i, exp, i)
    case d: Double => dpow(d, exp)
  }

  class MassActionReaction(
    val lhs: Multiset, val rhs: Multiset, val k: () => Double)
      extends KineticReaction {
    def rate = (for ((s, n) <- lhs) yield pow(s.value, n)).product * k()
  }

  class RateLawReaction(
    val lhs: Multiset, val rhs: Multiset, val k: () => Double)
      extends KineticReaction {
    def rate = k()
  }
}

