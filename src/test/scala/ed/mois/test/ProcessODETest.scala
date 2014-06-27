package ed.mois.test

import java.lang.Math.PI
import org.scalatest.FlatSpec
import ed.mois.ProcessODE
import ed.mois.ResourceConversions._

object exode extends ProcessODE("exode") {
  integral(
    resource(0.0, "ex:y1"),
    resource(1.0, "ex:y2")
  )
  def computeDerivatives(t: Double, y: Array[Double], ẏ: Array[Double]) {
    ẏ(0) = 1.0 - y(1)
    ẏ(1) = y(0) - 1.0
  }
}

/*
 * Directly calculated ODE system from Dominik's stuff
 */ 
object sampleODE extends ProcessODE("sample") {
  integral(
    resource(25.0, "ex:x1"),
    resource(50.0, "ex:x2")
  )
  def computeDerivatives(t: Double, y: Array[Double], ẏ: Array[Double]) {
    ẏ(0) = -0.3*y(0) - 0.4*y(1)
    ẏ(1) = -0.5*y(0) - 0.8*y(1)
  }
}

class ODEProcessTest extends FlatSpec {
  "ode process" should "integrate" in {
    var dy = exode(0, PI/2)
    assert(1 - exode.y(0) < 0.0001)
    assert(exode.y(1) < 0.0001) 
  }

  "sample ode" should "give dominik's expected results" in {
    println(sampleODE)
    sampleODE(0, 50.0)
    println(sampleODE)
  }
}
