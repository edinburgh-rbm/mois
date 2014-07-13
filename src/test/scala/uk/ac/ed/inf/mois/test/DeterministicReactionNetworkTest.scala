package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.{DeterministicReactionNetwork, Accumulator,
  DoubleVarIntf => D}

import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.chart.{ChartFactory, ChartUtilities}
import org.jfree.chart.plot.PlotOrientation

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

class DeterministicReactionNetworkTest extends FlatSpec with Matchers {

  object Brusselator
      extends DeterministicReactionNetwork("Brusselator") {

    val A = Specie("A")
    val B = Specie("B")
    val X = Specie("X")
    val Y = Specie("Y")

    reactions(
      A -> X + A at 1.0,
      2(X) + Y -> 3(X) at 1.0,
      B + X -> B + Y at 1.0,
      X -> () at 1.0
    )
  }

  // Use approximate equality in `should equal`
  val precision = 1e-3
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  "Brusselator" should "give expected results" in {

    Brusselator.A := 1.0
    Brusselator.B := 1.7
    Brusselator.X := 1.0
    Brusselator.Y := 1.0
    val acc = new Accumulator
    Brusselator.addStepHandler(acc)
    Brusselator.step(0, 50)

    // TODO: Move this into a uk.ac.ed.inf.mois.Plotting
    // Also maybe there should be an Accumulator that stores the
    // values directly on XYSeries for memory-efficiency.
    // Plot
    val series = (for (v <- Brusselator.vars) yield
      (v.meta, new XYSeries(v.meta))).toMap
    for ((t, vs) <- acc.history; v <- vs if v.isInstanceOf[D])
      series(v.meta).add(t, v.asInstanceOf[D].value)
    val dataset = new XYSeriesCollection
    for ((v, ss) <- series)
      dataset.addSeries(ss)
    val chart = ChartFactory.createXYLineChart(
      "Brusselator", "Time", "Concentration", dataset,
      PlotOrientation.VERTICAL, true, true, false)
    ChartUtilities.saveChartAsJPEG(
      new java.io.File("brusselator.jpg"), chart, 800, 600)

    // Tests
    Brusselator.X.value should equal (1.0)
    Brusselator.Y.value should equal (1.7)
  }
}

