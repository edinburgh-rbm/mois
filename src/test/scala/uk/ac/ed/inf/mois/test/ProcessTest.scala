package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.{Process, Var}
import uk.ac.ed.inf.mois.Conversions._

import org.scalatest.{FlatSpec, Matchers}

class ProcessTest extends FlatSpec with Matchers {

  object P1 extends Process("P1") {
    val x1 = Var(2.0, "ex:x1")
    val x2 = Var(3.0, "ex:x2")
    val x3 = Var(true, "ex:x3")
    def step(t: Double, tau: Double) {
      x1 := t + x1 * x2 * tau
      if (t % 2 != 0) x3 := !x3
      else x2 := x2 * x2
    }
  }

  "process" should "run and do arithmetic" in {
    // RHZ: Why did in the old assertions dx[Double](P1.x1)() == 4.0?

    // var dx = P1(0, 1)
    // assert(dx[Double](P1.x1)() == 4.0)
    // assert(dx[Double](P1.x2)() == 6.0)
    // assert(dx[Boolean](P1.x3)() == false)
    P1.step(0, 1)
    P1.x1.value should be (6.0)
    P1.x2.value should be (9.0)
    P1.x3.value should be (true)

    // dx = P1(1, 1)
    // assert(dx[Double](P1.x1)() == 49.0)
    // assert(dx[Double](P1.x2)() == 0.0)
    // assert(dx[Boolean](P1.x3)() == true)
    P1.step(1, 1)
    P1.x1.value should be (55.0)
    P1.x2.value should be (9.0)
    P1.x3.value should be (false)
  }

  // it should "keep referring to same variables" in {
  //   val x1 = Var(1.0, "ex:x1")
  //   P1.x1 should equal (55.0)
  //   // assert(P1.x1().value.asInstanceOf[Double] == 55.0)

  //   // put a new reference into the state
  //   P1.state += x1
  //   assert(P1.x1().value.asInstanceOf[Double] == 1.0)
  // }
}

