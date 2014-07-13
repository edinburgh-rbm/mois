package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.{VarMeta, PopulationBasedReactionNetwork}

import org.scalatest.{FlatSpec, Matchers}

class ReactionNetworkTest extends FlatSpec with Matchers {

  object ReactionNetwork extends PopulationBasedReactionNetwork {

    val name = "ReactionNetwork"

    class Reaction(val lhs: Multiset, val rhs: Multiset)
       extends ReactionIntf

    object Reaction extends ReactionFactory {
      def apply(lhs: Multiset, rhs: Multiset) =
        new Reaction(lhs, rhs)
    }

    // -- Process methods --

    def step(t: Double, dt: Double) { }

    // -- Brusselator --

    val A = Specie("A")
    val B = Specie("B")
    val D = Specie("D")
    val E = Specie("E")
    val X = Specie("X")
    val Y = Specie("Y")

    val r1  = A -> X
    val r2a = 2 * X + Y -> 3 * X
    val r2b = Y + X + X -> X * 2 + X
    val r3  = B + X -> Y + D
    val r4  = X -> E
  }

  // -- Tests --

  import ReactionNetwork._

  "Multisets" should "be constructed by enumerating species" in {
    Multiset(X) should be (Map(X -> 1))
    Multiset(X, X) should be (Map(X -> 2))
    Multiset(X, X, Y, X, Y) should be (Map(X -> 3, Y -> 2))
  }

  it should "add species" in {
    (Multiset.empty + X) should be (Multiset(X))
    (Multiset.empty + X + X) should be (Multiset(X, X))
  }

  it should "substract species" in {
    val m1 = Multiset(X, X, Y, X, Y) - X
    m1 should be (Multiset(X, X, Y, Y))
    (m1 - Y) should be (Multiset(X, X, Y))
  }

  it should "be equal regardless of ordering" in {
    val m1 = Multiset(X, Y, X, Y)
    m1 should be (Multiset(Y, X, Y, X))
    m1 should be (Multiset(X, X, Y, Y))
    m1 should be (Multiset(Y, Y, X, X))
  }

  "Reactions" should "be defined in a natural syntax" in {
    r1 should be (Reaction(Multiset(A), Multiset(X)))
    val r2 = Reaction(Multiset(X, X, Y), Multiset(X, X, X))
    r2a should be (r2)
    r2b should be (r2)
    r3 should be (Reaction(Multiset(B, X), Multiset(Y, D)))
    r4 should be (Reaction(Multiset(X), Multiset(E)))
  }

  it should "be equal regardless of ordering" in {
    r2a should equal (r2b)
  }
}

