package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.{Accumulator, Process, StepHandler, TsvWriter, Var}

import org.scalatest.{FlatSpec, Matchers}

class StepHandlerTest extends FlatSpec with Matchers {

  object p extends Process("p") {
    val x1 = Var(0, "ex:x1")
    state += x1
    def step(t: Double, tau: Double) {
      x1 := x1 + 1
    }
  }

  "accumulator" should "accumulate state" in {
    val acc = new Accumulator
    p.addStepHandler(acc)

    acc.handleStep(0, p.state)

    p(0, 1)
    p(1, 1)
    p(2, 1)

    acc(0.0)(p.x1).value should be (0)
    acc(1.0)(p.x1).value should be (1)
    acc(2.0)(p.x1).value should be (2)
    acc(3.0)(p.x1).value should be (3)
  }
}

class TsvWriterTest extends FlatSpec with Matchers {

  object p extends Process("p") {
    // purposely define them in the "wrong" order
    val x2 = Var(0, "ex:x2")
    val x1 = Var(0, "ex:x1")
    state += x2
    state += x1
    def step(t: Double, tau: Double) {
      x1 := x1 + 1
      x2 := 2*x1
    }
  }

  "file output" should "write tsv" in {
    val buffer = new java.io.StringWriter
    val fout = new TsvWriter(buffer)

    p.addStepHandler(fout)

    fout.handleStep(0, p.state)

    p(0, 1)
    p(1, 1)
    p(2, 1)

    val expected = 
"""0.0	0	0
1.0	1	2
2.0	2	4
3.0	3	6
"""
    buffer.toString should be (expected)
  }
}

