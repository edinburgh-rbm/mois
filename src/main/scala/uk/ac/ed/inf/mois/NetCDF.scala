/*
 *  MOIS: NetCdf file IO
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

import scala.language.existentials
import scala.language.implicitConversions
import scala.collection.mutable

import spire.algebra.Rig
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._

import ucar.nc2
import ucar.ma2

/**
 * A StepHandler may be added to a `Process`. It then gets called at
 * the conclusion of each step with the end time and the state.
 */
class NetCdfWriter(filename: String) extends StepHandler {
  private var cdf: nc2.NetcdfFileWriter = null
  private var origin: Array[Int] = null
  private var time: nc2.Variable = null
  private var _writers = mutable.ArrayBuffer.empty[VarCdfWriter]
  private val writers = _writers.toArray

  def init(t: Double, proc: Process) {
    cdf = create(proc)
    origin = Array.fill[Int](proc.dimensions.size + 1)(0)
    handleStep(t, proc)
  }

  def create(proc: Process): nc2.NetcdfFileWriter = {
    import NetCdfImplicits._

    val fp = nc2.NetcdfFileWriter.createNew(
      nc2.NetcdfFileWriter.Version.netcdf3,
      filename, null)

    // add user-defined annotations
    proc.addCdfAnnotations(fp)

    // add some extra metadata to the file
    val now = java.util.Calendar.getInstance().getTime().toString
    fp.addGroupAttribute(null, new nc2.Attribute("created", now))

    // add the time dimension, of course
    val timeDim = fp.addUnlimitedDimension("time")
    time = fp.addVariable(null, "time", ma2.DataType.DOUBLE, "time")

    // calculate the size, and make the buffers that we'll need
    val shapeBuf = proc.dimensions.keys.toSeq.sorted
      .foldLeft(Array(0))((z, k) => z :+ proc.dimensions(k))
    val shape = Array.fill(proc.dimensions.size+1)(1) //shapeBuf.toArray

    val byteData = new ma2.ArrayByte(shape)
    val shortData = new ma2.ArrayShort(shape)
    val intData = new ma2.ArrayInt(shape)
    val longData = new ma2.ArrayLong(shape)
    val floatData = new ma2.ArrayFloat(shape)
    val doubleData = new ma2.ArrayDouble(shape)
    val booleanData = new ma2.ArrayBoolean(shape)

    val state = proc.state

    // add the dimensions
    def addDim(op: NetCdfOps, data: ma2.Array, length: Int) = {
      val cv = op.addAsDimension(fp, length)
      op.dimWriter(fp, cv, data)
    }

    proc.dimensions.keys.toSeq.sorted map { meta =>
      val length = proc.dimensions(meta)
      if (meta.rig == Rig[Byte])
        _writers += addDim(state.getIndex[Byte](meta), byteData, length)
      else if (meta.rig == Rig[Short])
        _writers += addDim(state.getIndex[Short](meta), shortData, length)
      else if (meta.rig == Rig[Int])
        _writers += addDim(state.getIndex[Int](meta), intData, length)
      else if (meta.rig == Rig[Long])
        _writers += addDim(state.getIndex[Long](meta), longData, length)
      else if (meta.rig == Rig[Float])
        _writers += addDim(state.getIndex[Float](meta), floatData, length)
      else if (meta.rig == Rig[Double])
        _writers += addDim(state.getIndex[Double](meta), doubleData, length)
      else if (meta.rig == Rig[Boolean])
        _writers += addDim(state.getIndex[Boolean](meta), booleanData, length)
      else
        throw new IllegalArgumentException("Only numeric types supported as dimensions for now")
      proc.dimensions(meta) = 0
    }

    // add the regular variables
    val dims = "time " + proc.dimensions.keys.toSeq.sorted.mkString(" ")
    def notDim(meta: VarMeta) = !(proc.dimensions contains meta)
    def notDims[T](implicit rig: Rig[T]) = proc.state.getMeta[T].filter(notDim _)

    def addVar(op: NetCdfOps, data: ma2.Array) = {
      val cv = op.addAsVariable(fp, dims)
      op.varWriter(fp, cv, data)
    }

    notDims[Byte]
      .map(m => addVar(proc.state.getIndex[Byte](m), byteData))
      .foldLeft(_writers)((z, w) => z += w)
    notDims[Short]
      .map(m => addVar(proc.state.getIndex[Short](m), byteData))
      .foldLeft(_writers)((z, w) => z += w)
    notDims[Int]
      .map(m => addVar(proc.state.getIndex[Int](m), byteData))
      .foldLeft(_writers)((z, w) => z += w)
    notDims[Long]
      .map(m => addVar(proc.state.getIndex[Long](m), byteData))
      .foldLeft(_writers)((z, w) => z += w)
    notDims[Float]
      .map(m => addVar(proc.state.getIndex[Float](m), byteData))
      .foldLeft(_writers)((z, w) => z += w)
    notDims[Boolean]
      .map(m => addVar(proc.state.getIndex[Boolean](m), byteData))
      .foldLeft(_writers)((z, w) => z += w)

    // create the file
    fp.create()
    fp
  }

  /**
   * If we were clever, we would buffer values and the write them out
   * in batches.
   */
  def handleStep(t: Double, proc: Process) {
    val ddd = new ma2.ArrayDouble(Array(1))
    // first do time
    ddd.setDouble(0, t)
    cdf.write(time, origin, ddd)

    writers.foldLeft(1)((off, w) => w(origin, off))

    origin(0) += 1
  }

  override def reset(t: Double, proc: Process) {
    origin(0) = 0
    var off = 0
    for (meta <- proc.dimensions.keys.toSeq.sorted) {
      off += 1
      origin(off) = proc.dimensions(meta)
    }
  }

  override def finish {
    cdf.close()
    cdf = null
  }
}

abstract class VarCdfWriter {
  def apply(origin: Array[Int], offset: Int): Int
}

abstract class NetCdfOps(i: Var[_]) {
  val cdfType: ma2.DataType
  def cdfArray(shape: Array[Int]): ma2.Array
  def setData(data: ma2.Array, idx: ma2.Index): Unit

  def addAsVariable(fp: nc2.NetcdfFileWriter, dims: String): nc2.Variable = {
    val cv = fp.addVariable(null, i.meta.toString, cdfType, dims)
    addCdfAnnotations(cv)
    cv
  }

  def addAsDimension(fp: nc2.NetcdfFileWriter, length: Int): nc2.Variable = {
    fp.addDimension(null, i.meta.toString, length)
    val cv = fp.addVariable(null, i.meta.toString, cdfType, i.meta.toString)
    addCdfAnnotations(cv)
    cv
  }

  def addCdfAnnotations(cv: nc2.Variable): Unit = {
    import scala.collection.JavaConverters._
    for ((k, v) <- i.meta.annotations) {
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

  def dimWriter(
    fp: nc2.NetcdfFileWriter, cv: nc2.Variable, data: ma2.Array
  ): VarCdfWriter = {
    val buf = cdfArray(Array(1))
    new VarCdfWriter {
      def apply(origin: Array[Int], offset: Int) = {
        setData(data, new ma2.Index1D(Array(0)))
        fp.write(cv, Array(origin(offset)), buf)
        offset + 1
      }
    }
  }

  def varWriter(
    fp: nc2.NetcdfFileWriter, cv: nc2.Variable, data: ma2.Array
  ): VarCdfWriter = {
    new VarCdfWriter {
      def apply(origin: Array[Int], offset: Int) = {
        setData(data, data.getIndex)
        fp.write(cv, origin, data)
        offset
      }
    }
  }
}

class ByteToCdf(b: Var[Byte]) extends NetCdfOps(b) {
  val cdfType = ma2.DataType.BYTE
  def cdfArray(shape: Array[Int]) = new ma2.ArrayByte(shape)
  def setData(a: ma2.Array, idx: ma2.Index) = a.setByte(idx, b)
}

class IntToCdf(i: Var[Int]) extends NetCdfOps(i) {
  val cdfType = ma2.DataType.INT
  def cdfArray(shape: Array[Int]) = new ma2.ArrayInt(shape)
  def setData(a: ma2.Array, idx: ma2.Index) = a.setInt(idx, i)
}

class LongToCdf(l: Var[Long]) extends NetCdfOps(l) {
  val cdfType = ma2.DataType.LONG
  def cdfArray(shape: Array[Int]) = new ma2.ArrayLong(shape)
  def setData(a: ma2.Array, idx: ma2.Index) = a.setLong(idx, l)
}

class ShortToCdf(s: Var[Short]) extends NetCdfOps(s) {
  val cdfType = ma2.DataType.SHORT
  def cdfArray(shape: Array[Int]) = new ma2.ArrayShort(shape)
  def setData(a: ma2.Array, idx: ma2.Index) = a.setShort(idx, s)
}

class FloatToCdf(f: Var[Float]) extends NetCdfOps(f) {
  val cdfType = ma2.DataType.FLOAT
  def cdfArray(shape: Array[Int]) = new ma2.ArrayFloat(shape)
  def setData(a: ma2.Array, idx: ma2.Index) = a.setFloat(idx, f)
}

class DoubleToCdf(d: Var[Double]) extends NetCdfOps(d) {
  val cdfType = ma2.DataType.DOUBLE
  def cdfArray(shape: Array[Int]) = new ma2.ArrayDouble(shape)
  def setData(a: ma2.Array, idx: ma2.Index) = a.setDouble(idx, d)
}

class BooleanToCdf(b: Var[Boolean]) extends NetCdfOps(b) {
  val cdfType = ma2.DataType.BOOLEAN
  def cdfArray(shape: Array[Int]) = new ma2.ArrayBoolean(shape)
  def setData(a: ma2.Array, idx: ma2.Index) = a.setBoolean(idx, b)
}

class ProcToCdf(proc: Process) {
  def addCdfAnnotations(fp: nc2.NetcdfFileWriter): Unit = {
    import scala.collection.JavaConverters._
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
  }
}

object NetCdfImplicits {
  implicit final def byteImplicits(b: Var[Byte]): NetCdfOps = new ByteToCdf(b)
  implicit final def intImplicits(i: Var[Int]): NetCdfOps = new IntToCdf(i)
  implicit final def longImplicits(l: Var[Long]): NetCdfOps = new LongToCdf(l)
  implicit final def shortImplicits(s: Var[Short]): NetCdfOps = new ShortToCdf(s)
  implicit final def floatImplicits(f: Var[Float]): NetCdfOps = new FloatToCdf(f)
  implicit final def doubleImplicits(d: Var[Double]): NetCdfOps = new DoubleToCdf(d)
  implicit final def booleanImplicits(b: Var[Boolean]): NetCdfOps = new BooleanToCdf(b)
  implicit final def procImplicits(p: Process): ProcToCdf = new ProcToCdf(p)
}
