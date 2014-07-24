package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.{DeterministicReactionNetwork, PlotFileWriter}

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

class CatalyticReactionNetworkTest extends FlatSpec with Matchers {

  object GbKl extends DeterministicReactionNetwork("Goldbeter-Koshland") {

    val A = Specie("A")
    val B = Specie("B")
    val X = Specie("X")
    val Y = Specie("Y")

    addStepHandler(new PlotFileWriter("goldbeter-koshland.png"))

    reactions(
      A -> B catalysedBy X using MM(1, 1, 1),
      B -> A catalysedBy Y using MM(1, 1, 1)
    )
  }

  // Use approximate equality in `should equal`
  val precision = 1e-3
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "Goldbeter-Koshland" should "give expected results" in {

    import GbKl._

    A := 1.0
    B := 1.0
    X := 1.0
    Y := 1.0
    step(0, 50)

    val XA = enzymeComplex(X, A)
    val YB = enzymeComplex(Y, B)

    rxns should equal (Seq(
      A + X -> XA at 1,
      XA -> A + X at 1,
      XA -> B + X at 1,
      B + Y -> YB at 1,
      YB -> B + Y at 1,
      YB -> Y + A at 1))

    (X.value + XA.value) should equal (1.0)
    (Y.value + YB.value) should equal (1.0)
    (A.value + XA.value) should equal (1.0)
    (B.value + YB.value) should equal (1.0)
  }
}

