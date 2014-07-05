package uk.ac.ed.inf.mois.test

import org.scalatest.{FlatSpec, Matchers}

import uk.ac.ed.inf.mois.{Process, ProcessGroup, ProcessODE, Var}
import uk.ac.ed.inf.mois.NaiveScheduler

/** Directly transcribed ODE system from Dominik's stuff. */
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

/** Version of same that does not use Euler's method and instead
  * uses whatever the apache commons math suite says is best.
  */
object sampleApache1 extends ProcessODE("sampleApache1") {
  val x1 = Var(25.0, "ex:x1")
  val x2 = Var(50.0, "ex:x2")
  d(x1) := -0.3*x1 - 0.4*x2
}

object sampleApache2 extends ProcessODE("sampleApache2") {
  val x1 = Var(25.0, "ex:x1")
  val x2 = Var(50.0, "ex:x2")
  d(x2) := -0.5*x1 - 0.8*x2
}

/** Run the two versions of the system of ODEs with the NaiveScheduler. */
class NaiveSchedulerTest extends FlatSpec with Matchers {
  val precision = 1e-4

  "sample ode system" should "integrate using Euler's method" in {
    val pg = new ProcessGroup("naive euler")
    pg.scheduler = new NaiveScheduler(0.0001)
    val x1 = Var(0.0, "ex:x1")
    val x2 = Var(0.0, "ex:x2")

    pg.step(0, 50)

    pg.state(sampleEuler1.x1).value should equal (-0.1398)
    pg.state(sampleEuler1.x2).value should equal (0.0916)
  }

  it should "integrate using the apache ODE library too" in {
    val pg = new ProcessGroup("naive apache")
    pg.scheduler = new NaiveScheduler(0.01)
    val x1 = Var(0.0, "ex:x1")
    val x2 = Var(0.0, "ex:x2")

    pg.step(0, 50)

    pg.state(sampleApache1.x1).value should equal (-0.1398)
    pg.state(sampleApache1.x2).value should equal (0.0916)
  }
}
