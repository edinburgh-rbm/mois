package uk.ac.ed.inf.mois

/*
 * A `ProcessGroup` is a list of `Process`es and a `Scheduler`. It presents the
 * same interface as a `Process` and so hierarchies of them can be built.
 */
abstract class ProcessGroup(name: String) extends Process(name) {
  var processes = List[Process]()
  val scheduler: Scheduler

  /*
   * The += operator adds a process to the group
   */
  def +=(proc: Process) = {
    processes = proc :: processes
    // this loop unifies the underlying state variables between
    // the process and the process group
    for ((k,v) <- proc.state) {
      if (state contains k)
	// the group already has this variable, give it to the process
	proc.state += state(k)
      else
	// add to the group state
	state += v
    }
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
    scheduler(t, tau, state, processes:_*)
  }
}