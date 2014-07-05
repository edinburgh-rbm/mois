package uk.ac.ed.inf.mois.test

// import java.lang.Math.abs
import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.{ProcessODE, Var}
import uk.ac.ed.inf.mois.Conversions._

/** Directly calculated ODE system from Dominik's stuff. */
object sampleODE extends ProcessODE("sample") {
  // RHZ: This should look like this:
  // val x1 = Var("ex:x1")
  // val x2 = Var("ex:x2")
  // and that should construct `VarH`s that get their value from
  // the model that contains `sampleODE`.  In a way, models are like
  // databases of `Var`s (in addition to collections of `Process`es).
  val x1 = Var(25.0, "ex:x1")
  val x2 = Var(50.0, "ex:x2")
  val x3 = Var(0.0, "ex:x3")
  d(x1) := -0.3*x1 - 0.4*x2
  d(x2) := -0.5*x1 - 0.8*x2
  // XXXX this is wrong! Obviously it evaluates immediately and gives sin(0) always
  // RHZ: Yes, it doesn't work because we only support polynomial
  // functions on the rhs for now and sin(t) is not a polynomial.
  // The best way to realise something is wrong is by not importing
  // uk.ac.ed.inf.mois.Conversions._, the compiler error says it all.
  // d(x3) := java.lang.Math.sin(t)
}

class ODEProcessTest extends FlatSpec with Matchers {

  "sample ODE" should "give Dominik's expected results" in {

    // For `should equal` to use approximate equality
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
