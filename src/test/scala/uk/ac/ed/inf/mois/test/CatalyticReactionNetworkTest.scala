package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.reaction.DeterministicReactionNetwork
import uk.ac.ed.inf.mois.Model
import scala.language.reflectiveCalls
import spire.implicits._

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

object GbKl extends DeterministicReactionNetwork {
  annotate("description", "Goldbeter-Koshland")

  val A = Species("A")
  val B = Species("B")
  val X = Species("X")
  val Y = Species("Y")

  reactions(
    A -> B catalysedBy X using MM(1, 1, 1),
    B -> A catalysedBy Y using MM(1, 1, 1)
  )
}

class GbKlModel extends Model {
  val process = GbKl
  GbKl.A := 1.0
  GbKl.B := 1.0
  GbKl.X := 1.0
  GbKl.Y := 1.0
}

class CatalyticReactionNetworkTest extends FlatSpec with Matchers {

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
