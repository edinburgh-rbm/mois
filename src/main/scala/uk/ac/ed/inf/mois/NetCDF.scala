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
class NetCDFWriter(filename: String) extends StepHandler with VarConversions {
  private var cdf: nc2.NetcdfFileWriter = null
  private var origin: Array[Int] = null
  private var time: nc2.Variable = null
  private var cdfvars = mutable.Map.empty[Var[_], nc2.Variable]
  private var cdfindex: ma2.Index = null

  /**
   * We have a concept of "dimension" so we have to organise our variables
   * a bit differently.
   */
  private var doubleDims: Array[DoubleVarIntf] = null
  private var doubles: Array[DoubleVarIntf] = null
  private var floatDims: Array[FloatVar] = null
  private var floats: Array[FloatVar] = null
  private var intDims: Array[IntVar] = null
  private var ints: Array[IntVar] = null

  private var doubleData: ma2.ArrayDouble = null
  private var floatData: ma2.ArrayFloat = null
  private var intData: ma2.ArrayInt = null

  def init(t: Double, proc: BaseProcess) {
    cdf = create(proc)
    origin = Array.fill[Int](proc.dimensions.size + 1)(0)
    handleStep(t, proc)
  }

  def create(proc: BaseProcess): nc2.NetcdfFileWriter = {
    // needed for List.asJava for the annotations
    import scala.collection.JavaConverters._

    val fp = nc2.NetcdfFileWriter.createNew(
      nc2.NetcdfFileWriter.Version.netcdf3,
      filename, null)

    // add user-defined annotations
    for ((k, v) <- proc.annotations) {
      v match {
	case n: Number =>
	  fp.addGroupAttribute(null, new nc2.Attribute(k, n))
	case s: String =>
	  fp.addGroupAttribute(null, new nc2.Attribute(k, s))
	case l: List[_] =>
	  fp.addGroupAttribute(null, new nc2.Attribute(k, l.asJava))
	case _ =>
	  fp.addGroupAttribute(null, new nc2.Attribute(k, v.toString))
      }
    }

    // add some extra metadata to the file
    val now = java.util.Calendar.getInstance().getTime().toString
    fp.addGroupAttribute(null, new nc2.Attribute("created", now))

    // add the dimensions
    val timeDim = fp.addUnlimitedDimension("time")

    val shapeBuf = mutable.ArrayBuffer.empty[Int]

    time = fp.addVariable(null, "time", ma2.DataType.DOUBLE, "time")
    shapeBuf += 0

    val dbuf = mutable.ArrayBuffer.empty[DoubleVarIntf]
    val fbuf = mutable.ArrayBuffer.empty[FloatVar]
    val ibuf = mutable.ArrayBuffer.empty[IntVar]
    for (v <- proc.dimensions.keys.toSeq.sortBy(_.meta)) {
      val length = proc.dimensions(v)
      fp.addDimension(null, v.meta.toString, length)
      shapeBuf += length

      v.value match {
	case _: Double =>
	  val cv = fp.addVariable(null, v.meta.toString, ma2.DataType.DOUBLE, v.meta.toString)
	  cdfvars += v -> cv
	  dbuf +=  v.asInstanceOf[DoubleVarIntf]
	case _: Float =>
	  val cv = v -> fp.addVariable(null, v.meta.toString, ma2.DataType.FLOAT, v.meta.toString)
	  cdfvars += cv
	  fbuf += v.asInstanceOf[FloatVar]
	case _: Int =>
	  val cv = v -> fp.addVariable(null, v.meta.toString, ma2.DataType.INT, v.meta.toString)
	  cdfvars += cv
	  ibuf += v.asInstanceOf[IntVar]
	case _ =>
	  throw new IllegalArgumentException("Only numeric types supported as dimensions for now")
      }
      proc.dimensions(v) = 0
    }
    doubleDims = dbuf.toArray
    floatDims = fbuf.toArray
    intDims = ibuf.toArray

    val shape = Array.fill(proc.dimensions.size+1)(1) //shapeBuf.toArray
    val dims = "time " + (doubleDims.map(_.meta.toString) ++
			  floatDims.map(_.meta.toString) ++
			  intDims.map(_.meta.toString)).mkString(" ")

    def notDim(v: Var[_]) = !(proc.dimensions contains v)

    // XXX pain in the backside, have to dig through all the vars because
    // of the chemical reaction network defining its own sub-class of 
    // doubles!!
    //
    // it should be like this:
    //
    // doubles = proc.doubleVars.values.toSeq.filter(notDim).sortBy(_.meta).toArray
    //
    // instead it is like this:
    doubles = proc.allVars.values.toSeq
      .filter(_.isInstanceOf[DoubleVarIntf])
      .filter(notDim)
      .map(_.asInstanceOf[DoubleVarIntf])
      .sortBy(_.meta)
      .toArray
    floats = proc.floatVars.values.toSeq.filter(notDim).sortBy(_.meta).toArray
    ints = proc.intVars.values.toSeq.filter(notDim).sortBy(_.meta).toArray

    for (v <- doubles) {
      cdfvars += v -> fp.addVariable(null, v.meta.toString, ma2.DataType.DOUBLE, dims)
    }
    for (v <- floats) {
      cdfvars += v -> fp.addVariable(null, v.meta.toString, ma2.DataType.FLOAT, dims)
    }
    for (v <- ints) {
      cdfvars += v -> fp.addVariable(null, v.meta.toString, ma2.DataType.INT, dims)
    }

    doubleData = new ma2.ArrayDouble(shape)
    floatData = new ma2.ArrayFloat(shape)
    intData = new ma2.ArrayInt(shape)

    // annotate the variables
    for ((v, cv) <- cdfvars) {
      for ((k, v) <- v.meta.annotations) {
	v match {
	  case n: Number =>
	    cv.addAttribute(new nc2.Attribute(k, n))
	  case s: String =>
	    cv.addAttribute(new nc2.Attribute(k, s))
	  case l: List[_] =>
	    cv.addAttribute(new nc2.Attribute(k, l.asJava))
	  case _ =>
	    cv.addAttribute(new nc2.Attribute(k, v.toString))
	}
      }
    }

    fp.create()
    fp
  }

  /**
   * If we were clever, we would buffer values and the write them out
   * in batches.
   */
  def handleStep(t: Double, proc: BaseProcess) {
    val ddd = new ma2.ArrayDouble(Array(1))
    // first do time
    ddd.setDouble(0, t)
    cdf.write(time, origin, ddd)

    var off = 0
    // then do dimensions
    for (i <- 0 until doubleDims.size) {
      off += 1
      val v = doubleDims(i)
      ddd.setDouble(0, v)
      cdf.write(cdfvars(v), Array(origin(off)), ddd)
    }
    val fdd = new ma2.ArrayFloat(Array(0))
    for (i <- 0 until floatDims.size) {
      off += 1
      val v = floatDims(i)
      floatData.setFloat(0, v)
      cdf.write(cdfvars(v), Array(origin(off)), fdd)
    }
    val idd = new ma2.ArrayInt(Array(0))
    for (i <- 0 until intDims.size) {
      off += 1
      val v = intDims(i)
      intData.setInt(0, v)
      cdf.write(cdfvars(v), Array(origin(off)), idd)
    }

    // now write out variables
    val didx = doubleData.getIndex()
    for (v <- doubles) {
      doubleData.setDouble(didx, v)
      cdf.write(cdfvars(v), origin, doubleData)
    }

    val fidx = floatData.getIndex()
    for (v <- floats) {
      floatData.setFloat(fidx, v)
      cdf.write(cdfvars(v), origin, floatData)
    }

    val iidx = intData.getIndex()
    for (v <- ints) {
      intData.setInt(iidx, v)
      cdf.write(cdfvars(v), origin, intData)
    }

    origin(0) += 1
  }

  override def reset(t: Double, proc: BaseProcess) {
    origin(0) = 0
    var off = 0
    for (v <- doubleDims) {
      off += 1
      origin(off) = proc.dimensions(v)
    }
    for (v <- floatDims) {
      off += 1
      origin(off) = proc.dimensions(v)
    }
    for (v <- intDims) {
      off += 1
      origin(off) = proc.dimensions(v)
    }
  }

  override def finish {
    cdf.close()
    cdf = null
  }
}
