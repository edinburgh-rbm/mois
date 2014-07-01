package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.{Accumulator, Process, StepHandler, Var}
import uk.ac.ed.inf.mois.Conversions._

import org.scalatest.FlatSpec

class StepHandlerTest extends FlatSpec {
  object p extends Process("p") {
    val x1 = Var(0, "ex:x1")
    def step(t: Double, tau: Double) {
      x1 := x1 + 1
    }
  }

  "accumulator" should "accumulate state" in {
    val acc = new Accumulator
    p.stepHandler = acc

    p(0, 1)
    p(1, 1)
    p(2, 1)

    assert(acc(1.0)(p.x1)().asInstanceOf[Int] == 1)
    assert(acc(2.0)(p.x1)().asInstanceOf[Int] == 2)
    assert(acc(3.0)(p.x1)().asInstanceOf[Int] == 3)
  }
}

