package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.{DeterministicReactionNetwork, Accumulator,
			  Model, DoubleVarIntf => D}

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

object Brusselator
  extends DeterministicReactionNetwork("Brusselator") {

  val A = Specie("A")
  val B = Specie("B")
  val X = Specie("X")
  val Y = Specie("Y")

  reactions(
    A -> X + A at 1.0,
    2(X) + Y -> 3(X) at 1.0,
    B + X -> B + Y at 1.0,
    X -> () at 1.0
  )
}

/**
 * BrusselatorModel is used generically in test:run because it is a
 * decent model to test things like graphing out.
 */
class BrusselatorModel extends Model {
  val process = Brusselator
  Brusselator.A := 1.0
  Brusselator.B := 1.7
  Brusselator.X := 1.0
  Brusselator.Y := 1.0
}

class DeterministicReactionNetworkTest extends FlatSpec with Matchers {

  // Use approximate equality in `should equal`
  val precision = 1e-3
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "Brusselator" should "give expected results" in {

    val acc = new Accumulator
    Brusselator.addStepHandler(acc)
    Brusselator.step(0, 50)

    // Tests
    Brusselator.X.value should equal (1.0)
    Brusselator.Y.value should equal (1.7)
  }
}

