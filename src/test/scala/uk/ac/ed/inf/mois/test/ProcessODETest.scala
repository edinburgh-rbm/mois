package uk.ac.ed.inf.mois.test

// import java.lang.Math.abs
import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.{ProcessODE, Var}
import uk.ac.ed.inf.mois.Conversions._

/** Directly calculated ODE system from Dominik's stuff. */
object sampleODE extends ProcessODE("sample") {
  val x1 = Var(25.0, "ex:x1")
  val x2 = Var(50.0, "ex:x2")
  val x3 = Var(0.0, "ex:x3")
  d(x1)/dt := -0.3*x1 - 0.4*x2
  d(x2)/dt := -0.5*x1 - 0.8*x2
  d(x3)/dt := java.lang.Math.sin(t)
}

class ODEProcessTest extends FlatSpec with Matchers {

  "sample ODE" should "give Dominik's expected results" in {

    // Use approximate equality in `should equal`
    val precision = 1e-4
    implicit val doubleEquality =
      TolerantNumerics.tolerantDoubleEquality(precision)

    sampleODE.x1.value should equal (25.0)
    sampleODE.x2.value should equal (50.0)

    // Integrate from t1 = 0 to t2 = 50
    sampleODE.step(0, 50)

    sampleODE.x1.value should equal (-0.1398)
    sampleODE.x2.value should equal (0.0916)

    // Integrate from t1 = 50 to t2 = 150
    sampleODE.step(50, 100)

    sampleODE.x1.value should equal (-0.0032)
    sampleODE.x2.value should equal (0.0021)

    // reset the initial conditions
    sampleODE.x1 := 25.0
    sampleODE.x2 := 50.0

    // make sure we get the same results
    sampleODE.step(0, 50.0)
    sampleODE.x1.value should equal (-0.1398)
    sampleODE.x2.value should equal (0.0916)

    println(s"known good ODE $sampleODE")
  }
}
