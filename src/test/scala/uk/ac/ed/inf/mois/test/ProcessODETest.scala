package uk.ac.ed.inf.mois.test

import java.lang.Math.abs
import org.scalatest.FlatSpec

import uk.ac.ed.inf.mois.{ProcessODE, Var}
import uk.ac.ed.inf.mois.Conversions._

/*
 * Directly calculated ODE system from Dominik's stuff
 */ 
object sampleODE extends ProcessODE("sample") {
  integral(
    Var(25.0, "ex:x1"),
    Var(50.0, "ex:x2")
  )
  def computeDerivatives(t: Double, y: Array[Double], ẏ: Array[Double]) {
    ẏ(0) = -0.3*y(0) - 0.4*y(1)
    ẏ(1) = -0.5*y(0) - 0.8*y(1)
  }
}

class ODEProcessTest extends FlatSpec {
  "sample ode" should "give dominik's expected results" in {
    //println(sampleODE)
    sampleODE(0, 50.0)
    //println(sampleODE)

    assert(abs(sampleODE.y(0) + 0.1398) < 0.0001)
    assert(abs(sampleODE.y(1) + (-0.0916)) < 0.0001)

    // run it again
    sampleODE(50.0, 100)
    //println(sampleODE)

    assert(abs(sampleODE.y(0) + 0.0032) < 0.0001)
    assert(abs(sampleODE.y(1) + (-0.0021)) < 0.0001)

    // reset the initial conditions through the state table
    // tabarnac de conversion d'etat!
    val x1 = sampleODE.state(Var(0.0, "ex:x1")).asInstanceOf[Var[Double]]
    val x2 = sampleODE.state(Var(0.0, "ex:x2")).asInstanceOf[Var[Double]]
    x1 := 25.0
    x2 := 50.0

    // make sure we get the same results
    //println(sampleODE)
    sampleODE(0, 50.0)

    assert(abs(sampleODE.y(0) + 0.1398) < 0.0001)
    assert(abs(sampleODE.y(1) + (-0.0916)) < 0.0001)

    println(s"known good ODE $sampleODE")
  }
}
