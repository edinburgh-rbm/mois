package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.{Process, Var}

import org.scalatest.{FlatSpec, Matchers}

class ProcessTest extends FlatSpec with Matchers {

  object p extends Process("p") {
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
    p.step(0, 1)
    p.x1.value should be (6.0)
    p.x2.value should be (9.0)
    p.x3.value should be (true)

    p.step(1, 1)
    p.x1.value should be (55.0)
    p.x2.value should be (9.0)
    p.x3.value should be (false)
  }

  it should "keep referring to same variables" in {
    val x1 = Var(1.0, "ex:x1")
    p.x1.value should equal (55.0)

    // put a new reference into the state
    p.state += x1
    p.state(x1).value should equal (1.0)
  }
}

