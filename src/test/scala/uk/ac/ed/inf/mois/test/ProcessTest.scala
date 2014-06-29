package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.Process
import uk.ac.ed.inf.mois.VarConversions._

import org.scalatest.FlatSpec

class ProcessTest extends FlatSpec {
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
    var dx = p(0, 1)
    assert(dx[Double](p.x1)() == 4.0)
    assert(dx[Double](p.x2)() == 6.0)
    assert(dx[Boolean](p.x3)() == false)

    dx = p(1, 1)
    assert(dx[Double](p.x1)() == 49.0)
    assert(dx[Double](p.x2)() == 0.0)
    assert(dx[Boolean](p.x3)() == true)
  }
}

