package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.{DeterministicReactionNetwork, Model, PlotFileWriter}

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

object GbKl extends DeterministicReactionNetwork("Goldbeter-Koshland") {

  val A = Specie("A") := 1.0
  val B = Specie("B") := 1.0
  val X = Specie("X") := 1.0
  val Y = Specie("Y") := 1.0
  
  reactions(
    A -> B catalysedBy X using MM(1, 1, 1),
    B -> A catalysedBy Y using MM(1, 1, 1)
  )
}

class GbKlModel extends Model {
  val process = GbKl
}
  
class CatalyticReactionNetworkTest extends FlatSpec with Matchers {

  // Use approximate equality in `should equal`
  val precision = 1e-3
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "Goldbeter-Koshland" should "give expected results" in {

    import GbKl._

    step(0, 5)

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

