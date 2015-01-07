package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.Model
import uk.ac.ed.inf.mois.ode.Apache
import uk.ac.ed.inf.mois.reaction.DeterministicReactionNetwork

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics
import spire.implicits._

class Brusselator extends DeterministicReactionNetwork[Double, Double] with Apache {

  // TODO: Write about this in mois-examples
  val A = Species("A")
  val B = Species("B")
  val X = Species("X")
  val Y = Species("Y")

  import uk.ac.ed.inf.mois.math.Multiset
  val a: Species = B
  val x : Multiset[Species] = B + X

  reactions(
    A --> X + A at 1.0,
    2(X) + Y --> 3(X) at 1.0,
    B + X --> B + Y at 1.0,
    X --> () at 1.0
  )
}

// catch type problem discovered by jwk
class RateAdaptiveReaction extends DeterministicReactionNetwork[Double, Double] with Apache {
  val A = Species("A")
  val B = Species("B")
  val k = Double("k")
  val r = Double("r")
  reactions( A --> B at (k * r) )
}

/**
 * BrusselatorModel is used generically in test:run because it is a
 * decent model to test things like graphing out.
 */
class BrusselatorModel extends Model {
  val process = new Brusselator
  override def init(t: Double) {
    super.init(t)
    import process._
    A := 1.0
    B := 1.7
    X := 1.0
    Y := 1.0
  }
}

class DeterministicReactionNetworkTest extends FlatSpec with Matchers {

  // Use approximate equality in `should equal`
  val precision = 1e-3
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "Brusselator" should "give expected results" in {
    val brusselator = new Brusselator
    brusselator.init(0)
    import brusselator._

    A := 1.0
    B := 1.7
    X := 1.0
    Y := 1.0

    brusselator.step(0, 50)

    // Tests
    X.value should equal (1.0)
    Y.value should equal (1.7)
  }
}
