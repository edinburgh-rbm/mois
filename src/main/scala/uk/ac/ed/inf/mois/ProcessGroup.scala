package uk.ac.ed.inf.mois

abstract class ProcessGroup(name: String) extends Process(name) {
  var processes = List[Process]()
  val scheduler: Scheduler

  def +=(proc: Process) = {
    processes = proc :: processes
    for ((k,v) <- proc.state) {
      if (state contains k)
	proc.state += state(k)
      else
	state += v
    }
    this
  }

  def step(t: Double, tau: Double) {
    scheduler(t, tau, state, processes:_*)
  }
}
