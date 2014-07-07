package uk.ac.ed.inf.mois.test

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.DiscreteProcess

case class Henon(a: Double, b: Double) extends DiscreteProcess("Henon") {
  val x = Double("ex:x")
  val y = Double("ex:y")
  n(x) := 1.0 - a * x*x + y
  n(y) := b * x
}


class DiscreteProcessTest extends FlatSpec with Matchers {
  "henon process" should "give correct results" in {
    // Use approximate equality in `should equal`
    val precision = 1e-4
    implicit val doubleEquality =
      TolerantNumerics.tolerantDoubleEquality(precision)

    val henon = new Henon(1.4, 0.3)
    import henon._

    henon.x := 0.0
    henon.y := 0.0

    henon.step(0, 1)

    henon.x.value should equal (1.0)
    henon.y.value should equal (0.0)

    henon.step(0, 5)
    
    henon.x.value should equal (0.3475)
    henon.y.value should equal (0.1663)
  }
}
