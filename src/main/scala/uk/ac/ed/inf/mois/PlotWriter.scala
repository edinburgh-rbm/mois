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
abstract class PlotWriter(varnames: String*)
    extends StepHandler with VarConversions {

  val series = mutable.Map.empty[Var[_], XYSeries]
  var title = "Untitled"
  var ylabel = ""

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

  def init(t: Double, proc: BaseProcess) {
    if (varnames.size == 0) { // try to plot everything!
      for (v <- proc.allVars.values
	   if (v.isInstanceOf[DoubleVarIntf] && !(proc.dimensions contains v))) {
        series += v -> new XYSeries(label(v))
      }
    } else {
      for (vname <- varnames) {
	val v = proc.allVars(VarMeta(vname))
	series += v -> new XYSeries(label(v))
      }
    }

    title = if (proc.annotations contains "title")
	      proc.annotations("title").toString
	    else
	      proc.name
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

class PlotFileWriter(filename: String, varnames: String*)
    extends PlotWriter(varnames:_*) {

  def handleStep(t: Double, proc: BaseProcess) {
    for ((v, ss) <- series) {
      ss.add(t, v.asInstanceOf[DoubleVarIntf].value)
    }
  }

  override def reset(t: Double, proc: BaseProcess) {
    for(ss <- series.values) {
      ss.clear
    }
  }

  override def finish {
    val fp = new java.io.File(filename)
    ChartUtilities.saveChartAsPNG(fp, chart, 800, 600)
    if (fp.isInstanceOf[java.io.Closeable])
      fp.asInstanceOf[java.io.Closeable].close
  }
}

class PlotGUIWriter(varnames: String*)
    extends PlotWriter(varnames:_*) {

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

  class GuiHandler(t: Double, ds: mutable.Map[Double, XYSeries], title: String)
      extends Runnable {
    override def run {
      for ((d, s) <- ds)
        s.add(t, d)
      dataset.seriesChanged(new SeriesChangeEvent(this))
      dimTitle.setText(title)
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

  override def init(t: Double, proc: BaseProcess) {
    super.init(t, proc)
    for (ss <- series.values) {
      ss.setNotify(false)
      ss.setMaximumItemCount(600)
    }
    java.awt.EventQueue.invokeLater(new Gui)
  }

  def handleStep(t: Double, proc: BaseProcess) {
    val title = proc.dimensions.keys.mkString(", ")
    val ds = for ((v, s) <- series) yield (v.asInstanceOf[DoubleVarIntf].value, s)
    java.awt.EventQueue.invokeLater(new GuiHandler(t, ds, title))
  }

  override def reset(t: Double, proc: BaseProcess) {
    java.awt.EventQueue.invokeLater(new GuiReset)
  }
}
