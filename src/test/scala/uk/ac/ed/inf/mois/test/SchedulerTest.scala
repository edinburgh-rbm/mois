package uk.ac.ed.inf.mois.test

import org.scalatest.FlatSpec

import uk.ac.ed.inf.mois.{Process, ProcessGroup, ProcessODE}
import uk.ac.ed.inf.mois.NaiveScheduler
import uk.ac.ed.inf.mois.Conversions._

/*
 * Directly copied ODE system from Dominik's stuff
 */ 
object sampleEuler1 extends Process("sampleEuler1") {
  val x1 = Var(25.0, "ex:x1")
  val x2 = Var(50.0, "ex:x2")

  def step(t: Double, tau: Double) {
    x1 += (-0.3*x1 - 0.4*x2) * tau
  }
}

object sampleEuler2 extends Process("sampleEuler2") {
  val x1 = Var(25.0, "ex:x1")
  val x2 = Var(50.0, "ex:x2")

  def step(t: Double, tau: Double) {
    x2 += (-0.5*x1 - 0.8*x2) * tau
  }
}

/*
 * Version of same that does not use Euler's method and instead
 * uses whatever the apache commons math suite says is best
 */ 
object sampleApache1 extends ProcessODE("sampleApache1") {
  integral(
    Var(25.0, "ex:x1")
  )
  val x2 = Var(50.0, "ex:x2")
  
  def computeDerivatives(t: Double, y: Array[Double], ẏ: Array[Double]) {
    ẏ(0) = -0.3*y(0) - 0.4*x2
  }
}

object sampleApache2 extends ProcessODE("sampleApache2") {
  val x1 = Var(25.0, "ex:x1")
  integral(
    Var(50.0, "ex:x2")
  )

  def computeDerivatives(t: Double, y: Array[Double], ẏ: Array[Double]) {
    ẏ(0) = -0.5*x1 - 0.8*y(0)
  }
}

class NaiveSchedulerTest extends FlatSpec {
  "sample ode processes (euler)" should "integrate" in {
    val pg = new ProcessGroup("naive euler") {
      val scheduler = new NaiveScheduler(0.0001)
    }

    pg += sampleEuler1
    pg += sampleEuler2

    pg(0, 50)

    println(pg)
  }

  "sample ode processes (apache)" should "integrate with apache ODE library too" in {
    val pg = new ProcessGroup("naive apache") {
      val scheduler = new NaiveScheduler(0.01)
    }

    pg += sampleApache1
    pg += sampleApache2

    pg(0, 50)

    println(pg)
  }

}
