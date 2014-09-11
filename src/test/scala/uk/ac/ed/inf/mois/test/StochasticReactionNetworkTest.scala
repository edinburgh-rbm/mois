package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.reaction.StochasticReactionNetwork

import scala.language.reflectiveCalls
import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics
import spire.implicits._

class StochasticReactionNetworkTest extends FlatSpec with Matchers {

  class GbKl extends StochasticReactionNetwork {
    annotate("long_name", "Goldbeter-Koshland")

    val A = Species("A")
    val B = Species("B")
    val X = Species("X")
    val Y = Species("Y")

    reactions(
      A -> B catalysedBy X using MM(1, 1, 1),
      B -> A catalysedBy Y using MM(1, 1, 1)
    )
  }

  "Goldbeter-Koshland" should "give expected results" in {
    val gbkl = new GbKl
    import gbkl._
    val XA = enzymeComplex(X, A)
    val YB = enzymeComplex(Y, B)
    gbkl.init(0)

    A := 10000
    B := 10000
    X := 10000
    Y := 10000

    val s = 1e-7
    for (i <- 0.0 until 1e-6 by s) {
      step(i, i+s)

      (X.value + XA.value) should equal (10000)
      (Y.value + YB.value) should equal (10000)
      (A.value + XA.value + B.value + YB.value) should equal (20000)
    }
  }
}
