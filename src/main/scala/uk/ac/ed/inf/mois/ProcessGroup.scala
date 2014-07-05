package uk.ac.ed.inf.mois

import scala.collection.mutable

/*
 * A `ProcessGroup` is a list of `Process`es and a `Scheduler`. It presents the
 * same interface as a `Process` and so hierarchies of them can be built.
 */
class ProcessGroup(name: String) extends Process(name) {
  var processes = mutable.ArrayBuffer.empty[Process]
  var scheduler: Scheduler = null

  /*
   * The += operator adds a process to the group
   */
  def +=(proc: Process) = {
    processes += proc
    // this loop unifies the underlying state variables between
    // the process and the process group
    proc.ints map { v => this.ints += v }
    proc.longs map { v => this.longs += v }
    proc.floats map { v => this.floats += v }
    proc.doubles map { v => this.doubles += v }
    proc.bools map { v => this.bools += v }
    this
  }

  /*
   * The -= operator removes a process from the group
   */
  def -=(proc: Process) = {
    // TODO: needed for process migration. Keeping state
    // coherent is important here
    this
  }

  /*
   * The `step` method of the `Process` interface calls the `Scheduler` on
   * the listof processes together with the group state table and time
   * parameters
   */
  def step(t: Double, tau: Double) {
    scheduler(t, tau, this, processes:_*)
  }
}
