/*
 *  MOIS: Step Handler
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

/**
 * A StepHandler may be added to a `Process`. It then gets called at
 * the conclusion of each step with the end time and the state.
 */
abstract class StepHandler {
  def init(t: Double, proc: Process)
  def handleStep(t: Double, proc: Process)
  def reset(t: Double, proc: Process) {}
  def finish {}
}

/**
 * The Accumulator is a trivial step handler that maintains
 * all state in a time-indexed dictionary in memory
 */
class Accumulator extends StepHandler {
  val history = mutable.ArrayBuffer.empty[(Double, State)]
  def handleStep(t: Double, proc: Process) {
    history += ((t, proc.state.deepCopy))
  }
  def init(t: Double, proc: Process) = handleStep(t, proc)
  // TODO: Should the Accumulator interpolate?
  // Q: How do we interpolate booleans?
  def apply[T](t: Double)(key: Var[T])(implicit rig: Rig[T]) = {
    // FIXME really stupid linear search
    val state = history.filter(_._1 <= t).last._2
    state.getVar(key.meta)
  }
}

/**
 * TsvWriter is a step handler that writes out data in tab-separated
 * form suitable for use with gnuplot and spreadsheets. It is initialised
 * with an writeable object such as a file handle or a memory buffer.
 * The implementation is naive and doesn't attempt to do any buffering
 * of writes itself.
 */
class TsvWriter(fp: java.io.Writer, sep: String = "\t")
    extends StepHandler {
  private val ordering = mutable.Map.empty[Rig[_], Array[Int]]
  def init(t: Double, proc: Process) {
    val rigs = proc.state.getTypes.sortBy(_.toString)
    for (rig <- rigs) {
      ordering(rig) = proc.state.getMeta(rig)
        .filter(m => (!m.flags.param && !m.flags.dimension))
        .sortBy(m => m.identifier)
        .map(m => proc.state.getMeta(rig).indexOf(m))
        .toArray
    }
    val metas = (for (rig <- rigs) yield ordering(rig)
      .map(i => proc.state.getMeta(rig)(i)).toArray)
      .foldLeft(mutable.ArrayBuffer.empty[VarMeta])((z, a) => z ++ a)
    fp.write(metas.map(x => x.identifier).mkString(sep) + "\n")
    handleStep(t, proc)
  }
  def handleStep(t: Double, proc: Process) {
    // apply a predictable ordering
    val rigs = proc.state.getTypes.sortBy(_.toString)
    val vars = (for (rig <- rigs) yield ordering(rig)
      .map(i => proc.state.get(rig)(i)).toArray)
      .foldLeft(mutable.ArrayBuffer.empty[Any])((z, a) => z ++ a)
    fp.write(vars.mkString(sep) + "\n")
  }
  override def reset(t: Double, proc: Process) {
    fp.write("\n")
    handleStep(t, proc)
  }
  override def finish = {
    if (fp.isInstanceOf[java.io.Closeable])
      fp.asInstanceOf[java.io.Closeable].close
  }
}
