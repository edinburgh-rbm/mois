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

import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.chart.{ChartFactory, ChartUtilities}
import org.jfree.chart.plot.PlotOrientation

/**
 * A StepHandler may be added to a `Process`. It then gets called at
 * the conclusion of each step with the end time and the state.
 */
class PlotWriter(filename: String, varnames: String*)
    extends StepHandler with VarConversions {

  val series = mutable.Map.empty[Var[_], XYSeries]
  var title = "Untitled"
  var ylabel = ""

  def init(t: Double, proc: BaseProcess) {
    def label(v: Var[_]): String = {
      if (v.meta.annotations contains "long_name")
	v.meta.annotations("long_name").toString
      else
	v.meta.identifier
    }

    if (varnames.size == 0) { // try to plot everything!
      for (v <- proc.allVars.values if v.isInstanceOf[DoubleVarIntf]) {
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

  def handleStep(t: Double, proc: BaseProcess) {
    for ((v, ss) <- series) {
      ss.add(t, v.asInstanceOf[DoubleVarIntf].value)
    }
  }

  override def reset(t: Double, proc: BaseProcess) {
    for (vm <- series.keys) {
      series(vm) = new XYSeries(vm)
    }
  }

  override def finish {
    val dataset = new XYSeriesCollection
    for ((v, ss) <- series.toSeq.sortBy(_._1.meta))
      dataset.addSeries(ss)
    val chart = ChartFactory.createXYLineChart(
      title, "Time", ylabel, dataset,
      PlotOrientation.VERTICAL, true, true, false)
    ChartUtilities.saveChartAsJPEG(
      new java.io.File(filename), chart, 800, 600
    )
  }
}
