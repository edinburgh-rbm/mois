package uk.ac.ed.inf.mois

import scala.collection.mutable

/**
 * A StepHandler may be added to a `Process`. It then gets called at
 * the conclusion of each step with the end time and the state.
 */
abstract class StepHandler {
  def init(t: Double, proc: BaseProcess)
  def handleStep(t: Double, proc: BaseProcess)
}

/**
 * The Accumulator is a trivial step handler that maintains
 * all state in a time-indexed dictionary in memory
 */
class Accumulator extends StepHandler {
  val history = mutable.Map.empty[Double, Seq[Var[_]]]
  def handleStep(t: Double, proc: BaseProcess) {
    history(t) = proc.state.map(_.copy).toList
  }
  def init(t: Double, proc: BaseProcess) = handleStep(t, proc)
  // TODO: Should the Accumulator interpolate?
  def apply(t: Double) = history(t)
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
  def init(t: Double, proc: BaseProcess) {
    val vars = (for (v <- proc.state) yield v).toSeq.sortBy(_.meta)
    fp.write("t" + sep + vars.map(x => x.meta.identifier).mkString(sep) + "\n")
    fp.write(t.toString + sep + vars.map(x => x.value).mkString(sep) + "\n")
  }
  def handleStep(t: Double, proc: BaseProcess) {
    // apply a predictable ordering
    val vars = (for (v <- proc.state) yield v).toSeq.sortBy(_.meta)
    fp.write(t.toString + sep + vars.map(x => x.value).mkString(sep) + "\n")
  }
}
