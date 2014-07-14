/*
 *  MOIS: NetCDF file IO
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
import ucar.nc2
import ucar.ma2

/**
 * A StepHandler may be added to a `Process`. It then gets called at
 * the conclusion of each step with the end time and the state.
 */
class NetCDFWriter(filename: String) extends StepHandler {
  private var cdf: nc2.NetcdfFileWriter = null
  private var cdfvars: Array[nc2.Variable] = null
  private var tick = 0

  def init(t: Double, proc: BaseProcess) {
    cdf = create(proc)
    handleStep(t, proc)
  }

  def create(proc: BaseProcess): nc2.NetcdfFileWriter = {
    val fp = nc2.NetcdfFileWriter.createNew(
      nc2.NetcdfFileWriter.Version.netcdf3,
      filename, null)
    val timeDim = fp.addUnlimitedDimension("time")

    val varlist = mutable.ArrayBuffer.empty[nc2.Variable]
    varlist += fp.addVariable(null, "time", ma2.DataType.DOUBLE, "time")
    for (v <- proc.doubleVars.values.toSeq.sortBy(_.meta)) {
      varlist += fp.addVariable(null, v.meta.toString, ma2.DataType.DOUBLE, "time")
    }
    for (v <- proc.floatVars.values.toSeq.sortBy(_.meta)) {
      varlist += fp.addVariable(null, v.meta.toString, ma2.DataType.FLOAT, "time")
    }
    for (v <- proc.intVars.values.toSeq.sortBy(_.meta)) {
      varlist += fp.addVariable(null, v.meta.toString, ma2.DataType.INT, "time")
    }
    cdfvars = varlist.toArray
    fp.create()
    fp
  }

  def handleStep(t: Double, proc: BaseProcess) {
    var offset = 1
    val origin = Array[Int](tick)

    val doubleData = new ma2.ArrayDouble(Array[Int](1))

    // first do time
    doubleData.setDouble(0, t)
    cdf.write(cdfvars(0), origin, doubleData)

    for (i <- 0 until proc.doubleVars.size) {
      val doubles = proc.doubleVars.values.toSeq.sortBy(_.meta).map(_.value)
      doubleData.setDouble(0, doubles(i))
      cdf.write(cdfvars(offset+i), origin, doubleData)
    }
    offset += proc.doubleVars.size

    val floatData = new ma2.ArrayFloat(Array[Int](1))
    for (i <- 0 until proc.floatVars.size) {
      val floats = proc.floatVars.values.toSeq.sortBy(_.meta).map(_.value)
      floatData.setFloat(0, floats(i))
      cdf.write(cdfvars(offset+i), origin, floatData)
    }
    offset += proc.floatVars.size

    val intData = new ma2.ArrayInt(Array[Int](1))
    for (i <- 0 until proc.intVars.size) {
      val ints = proc.intVars.values.toSeq.sortBy(_.meta).map(_.value)
      intData.setInt(0, ints(i))
      cdf.write(cdfvars(offset+i), origin, intData)
    }

    tick += 1
  }

  override def reset(t: Double, proc: BaseProcess) {
    tick = 0
  }

  override def finish {
    cdf.close()
    cdf = null
  }
}
