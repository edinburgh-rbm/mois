/*
 *  MOIS: Plotting Step Handler
 *  Copyright (C) 2014 University of Edinburgh School of Informatics
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package uk.ac.ed.inf.mois

import scala.collection.mutable

import spire.algebra.Rig
import spire.implicits._

import org.jfree.chart.{ChartFactory, ChartPanel, ChartUtilities}
import org.jfree.chart.{ChartMouseEvent, ChartMouseListener}
import org.jfree.chart.title.TextTitle
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.general.SeriesChangeEvent
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import javax.swing.{JFrame, WindowConstants}

/**
 * A StepHandler may be added to a `Process`. It then gets called at
 * the conclusion of each step with the end time and the state.
 */
abstract class PlotWriter extends StepHandler {
  private val rig = Rig[Double]
  val vars: Seq[Var[Double]]
  val series = mutable.Map.empty[Var[Double], XYSeries]
  var title = "Untitled"
  var ylabel = ""

  // RHZ: This would actually make for a good Var.toString method
  def label(v: Var[_]): String = {
    if (v.meta.annotations contains "long_name") {
      v.meta.annotations("long_name").toString +
        (if (v.meta.annotations contains "units")
          " (" + v.meta.annotations("units").toString + ")"
        else
          "")
    } else {
      v.meta.identifier
    }
  }

  def init(t: Double, proc: Process) {
    if (vars.size == 0) { // try to plot everything!
      for (v <- proc.state.getMeta[Double].map(proc.state.getVar[Double](_))) {
        if (!v.isParam && !v.isDimension) {
          series += v -> new XYSeries(label(v))
        }
      }
    } else {
      for (v <- vars) {
        series += v -> new XYSeries(label(v))
      }
    }

    if (proc.annotations contains "title")
      title = proc.annotations("title").toString
  }

  val dataset = new XYSeriesCollection
  lazy val chart = {
    for ((v, ss) <- series.toSeq.sortBy(_._1.meta))
      dataset.addSeries(ss)
    ChartFactory.createXYLineChart(
      title, "Time", ylabel, dataset,
      PlotOrientation.VERTICAL, true, true, false)
  }
}

class PlotFileWriter(val filename: String, val vars: Var[Double]*)
    extends PlotWriter {

  def handleStep(t: Double, proc: Process) {
    for ((v, ss) <- series) ss.add(t, v.value)
  }

  override def reset(t: Double, proc: Process) {
    for(ss <- series.values) ss.clear
  }

  override def finish {
    val fp = new java.io.File(filename)
    ChartUtilities.saveChartAsPNG(fp, chart, 800, 600)
    if (fp.isInstanceOf[java.io.Closeable])
      fp.asInstanceOf[java.io.Closeable].close
  }
}

class PlotGUIWriter(val vars: Var[Double]*) extends PlotWriter {

  val dimTitle = new TextTitle

  class Gui extends Runnable with ChartMouseListener {
    override def run {
      val frame = new JFrame("mois")
      frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
      frame.setSize(800, 600)

      chart.addSubtitle(dimTitle)

      val panel = new ChartPanel(chart)
      panel.setPreferredSize(new java.awt.Dimension(800, 600))
      panel.addChartMouseListener(this)

      frame.setContentPane(panel)

      frame.pack()
      frame.setVisible(true)
    }

    var running = true
    def chartMouseClicked(e: ChartMouseEvent) {
      running = !running
      chart.setNotify(running)
    }

    def chartMouseMoved(e: ChartMouseEvent) {}
  }

  class GuiHandle(t: Double, ds: mutable.Map[Double, XYSeries], title: String)
      extends Runnable {
    var count = 0
    override def run {
      for ((d, s) <- ds)
        s.add(t, d)
      if (count % 200 == 0) {
        dataset.seriesChanged(new SeriesChangeEvent(this))
        dimTitle.setText(title)
      }
      count += 1
    }
  }

  class GuiReset extends Runnable {
    override def run {
      for(ss <- series.values) {
        ss.clear
      }
      dataset.seriesChanged(new SeriesChangeEvent(this))
    }
  }

  class GuiFinish extends Runnable {
    override def run {
      dataset.seriesChanged(new SeriesChangeEvent(this))
    }
  }

  override def init(t: Double, proc: Process) {
    super.init(t, proc)
    for (ss <- series.values) {
      ss.setNotify(false)
      ss.setMaximumItemCount(600)
    }
    java.awt.EventQueue.invokeLater(new Gui)
  }

  def handleStep(t: Double, proc: Process) {
    val title = proc.dimensions.keys.mkString(", ")
    val ds = for ((v, s) <- series) yield (v.value, s)
    java.awt.EventQueue.invokeLater(new GuiHandle(t, ds, title))
  }

  override def reset(t: Double, proc: Process) {
    java.awt.EventQueue.invokeLater(new GuiReset)
  }

  override def finish {
    java.awt.EventQueue.invokeLater(new GuiFinish)
  }
}
