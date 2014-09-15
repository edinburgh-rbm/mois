package uk.ac.ed.inf.mois.test

import spire.implicits._

import uk.ac.ed.inf.mois.reaction.PopulationBasedReactionNetwork
import uk.ac.ed.inf.mois.math.Multiset

import org.scalatest.{FlatSpec, Matchers}

class ReactionNetworkTest extends FlatSpec with Matchers {

  object ReactionNetwork extends PopulationBasedReactionNetwork[Int] {

    val name = "ReactionNetwork"

    class Reaction(val lhs: Multiset[Species], val rhs: Multiset[Species])
       extends BaseReaction

    object Reaction extends ReactionFactory {
      def apply(lhs: Multiset[Species], rhs: Multiset[Species]) =
        new Reaction(lhs, rhs)
    }

    // -- Brusselator --

    val A = Species("A")
    val B = Species("B")
    val D = Species("D")
    val E = Species("E")
    val X = Species("X")
    val Y = Species("Y")

    val r1  = A --> X
    // XXX spire broke this syntax because of some nonsense about
    // multiplicative semi-groups. It might be possible to make it
    // work more elegantly by doing something spire-compatib
    // val r2a = 2 * X + Y -> 3 * X
    val r2a = 2(X) + Y --> 3(X)
    val r2b = Y + X + X --> X * 2 + X
    val r3  = B + X --> Y + D
    val r4  = X --> E
    init(0)
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
