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
    GbKl.A := 1.0
    GbKl.B := 1.0
    GbKl.X := 1.0
    GbKl.Y := 1.0
    GbKl.step(0, 50)

    GbKl.X.value should equal (1.0)
    GbKl.Y.value should equal (1.0)
    GbKl.A.value should equal (0.5)
    GbKl.B.value should equal (0.5)
  }
}

