package uk.ac.ed.inf.mois.test

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.{Process, ProcessGroup, ProcessODE}
import uk.ac.ed.inf.mois.sched.{NaiveScheduler, KarrScheduler}

/** Directly transcribed ODE system from Dominik's stuff. */
object sampleEuler1 extends Process("sampleEuler1") {
  val x1 = Double("ex:x1")
  val x2 = Double("ex:x2")

  def step(t: Double, tau: Double) {
    x1 += (-0.3*x1 - 0.4*x2) * tau
  }
}

object sampleEuler2 extends Process("sampleEuler2") {
  val x1 = Double("ex:x1")
  val x2 = Double("ex:x2")

  def step(t: Double, tau: Double) {
    x2 += (-0.5*x1 - 0.8*x2) * tau
  }
}

/** Version of same that does not use Euler's method and instead
  * uses whatever the apache commons math suite says is best.
  */
class SampleApache1 extends ProcessODE("sampleApache1") {
  val x1 = Double("ex:x1")
  val x2 = Double("ex:x2")
  d(x1) := -0.3*x1 - 0.4*x2
}

class SampleApache2 extends ProcessODE("sampleApache2") {
  val x1 = Double("ex:x1")
  val x2 = Double("ex:x2")
  d(x2) := -0.5*x1 - 0.8*x2
}

/** Run the two versions of the system of ODEs with the NaiveScheduler. */
class NaiveSchedulerTest extends FlatSpec with Matchers {

  // Use approximate equality in `should equal`
  val precision = 1e-3
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "sample ode system" should "integrate using Euler's method" in {
    val pg = new ProcessGroup("naive euler")
    import pg._

    pg.scheduler = new NaiveScheduler(0.001)
    pg += sampleEuler1
    pg += sampleEuler2

    val x1 = Double("ex:x1") := 25.0
    val x2 = Double("ex:x2") := 50.0

    pg.step(0, 50)

    x1.value should equal (-0.1398)
    x2.value should equal (0.0916)
  }

  it should "integrate using the apache ODE library too" in {
    val pg = new ProcessGroup("naive apache")
    import pg._

    pg.scheduler = new NaiveScheduler(0.005)
    pg += new SampleApache1
    pg += new SampleApache2

    val x1 = Double("ex:x1") := 25.0
    val x2 = Double("ex:x2") := 50.0

    pg.step(0, 50)

    x1.value should equal (-0.1398)
    x2.value should equal (0.0916)
  }
}

/** Run the two versions of the system of ODEs with the NaiveScheduler. */
class KarrSchedulerTest extends FlatSpec with Matchers {

  // Use approximate equality in `should equal`
  val precision = 1e-3
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "sample ode system" should "integrate using the apache ODE library" in {
    val pg = new ProcessGroup("naive apache")
    import pg._

    pg.scheduler = new KarrScheduler(0.1)
    pg += new SampleApache1
    pg += new SampleApache2

    Double("ex:x1") := 25.0
    Double("ex:x2") := 50.0

/*    import uk.ac.ed.inf.mois.TsvWriter
    val fp = new java.io.PrintWriter(
      new java.io.PrintWriter(new java.io.OutputStreamWriter(System.out, "UTF-8"))
    )
    val output = new TsvWriter(fp)
    pg.addStepHandler(output)
    output.init(0, pg)
*/
    pg.step(0, 50)

//    fp.flush()

    println(pg.toJSON)
  }
}
