package uk.ac.ed.inf.mois

import scala.collection.mutable.Map

/**
 * A StepHandler may be added to a `Process`. It then gets called at
 * the conclusion of each step with the end time and the state.
 */
abstract class StepHandler {
  def init(t: Double, state: State)
  def handleStep(t: Double, state: State)
}

/**
 * The Accumulator is a trivial step handler that maintains
 * all state in a time-indexed dictionary in memory
 */
class Accumulator extends StepHandler {
  var history = Map.empty[Double, State]
  def handleStep(t: Double, state: State) {
    history += t -> state.copy
  }
  def init(t: Double, state:State) {
    handleStep(t, state)
  }
  def apply(t: Double) = history(t)
}

/**
 * TsvWriter is a step handler that writes out data in tab-separated
 * form suitable for use with gnuplot and spreadsheets. It is initialised
 * with an writeable object such as a file handle or a memory buffer.
 * The implementation is naive and doesn't attempt to do any buffering
 * of writes itself.
 */
class TsvWriter(fp: java.io.Writer) extends StepHandler {
  def init(t: Double, state: State) {
    val vars = (for ((_, v) <- state) yield v)
      .toList.sortWith((a, b) => a.key < b.key)
    fp.write("t" + "\t" + vars.map(x => x.identifier).mkString("\t") + "\n")
  }
  def handleStep(t: Double, state: State) {
    // apply a predictable ordering
    val vars = (for ((_, v) <- state) yield v)
      .toList.sortWith((a, b) => a.key < b.key)
    fp.write(t.toString + "\t" + vars.map(x => x.value).mkString("\t") + "\n")
  }
}
