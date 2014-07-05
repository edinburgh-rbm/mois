package uk.ac.ed.inf.mois.sched

/*
 
import uk.ac.ed.inf.mois.{Process, Scheduler, NumericVar}

import scala.collection.mutable

class KarrScheduler(step: Double) extends Scheduler {
  // ds holds the demand, which is approximated by  the change per
  // unit time of each variable
  val ds = mutable.Map.empty[Process, State]
  def apply(t: Double, tau: Double, state: State, procs: Process*) {

    // we have no change or demand information so run for one step to
    // collect it
    if (ds.size == 0) {
      for (p <- procs) {
	// synthesize a new state. this disunifies the underlying variables!
	// yes, rhz, you told me so
	p.state ++= p.state.copy
	val dx = p(t, step)
	for (v <- dx) {
	  // we only do doubles for now because we can't properly divide
	  // other things
	  v.value match {
	    case d: Double => 
	      // XXX ugly arithmetic. Shoudl be
	      // v := state(v) * d / step
	      v.asInstanceOf[NumericVar[Double]] := state(v).value.asInstanceOf[Double] * d / step
	    case _ =>
	  }
	}
	// create the entry in our 
	ds += p -> dx
      }
    }

    // find out total demand for each variable
    val demand = new State
    for (v <- state) {
      demand += v
      v.value match {
	case d: Double =>
	  demand(v) := 0
	  for (p <- procs) {
	    // XXX tabarnac
	    // demand(v) += state(v) * ds(p)(v)
	    demand(v).asInstanceOf[NumericVar[Double]] += 
	      state(v).value.asInstanceOf[Double] * 
	      ds(p)(v).value.asInstanceOf[Double]
	  }
      }
    }

    // now let the process run in steps until tau
    var dt = 0.0
    while (dt < tau) {
      // make a copy of the state variables subject to contention
      val contended = new State
      for (v <- demand)
        contended(v) := 0.0

      for (p <- procs) {
	// adjust each process-local state variable according
	// to its allocated portion of that variable if necessary,
	// or copy the global one
	for (v <- p.state) {
	  if (demand contains v)
   	    // XXXX more uglier arithmetic
	    // p.state(v) := state(v) * ds(p)(v) / demand(v)
	    p.state(v).asInstanceOf[NumericVar[Double]] := 
   	      state(v).value.asInstanceOf[Double] *
	      ds(p)(v).value.asInstanceOf[Double] /
	      demand(v).value.asInstanceOf[Double]
	  else
	    p.state(v) := state(v)
	}

	// run the process for one step
	val dx = p(t + dt, step)
	// update the demand from that process, and the 
	// accumulated new values
	for (v <- ds(p)) {
	  // XXXX ffs.
	  // ds(p)(v) := state(v) * dx(v) / step
	  ds(p)(v).asInstanceOf[NumericVar[Double]] :=
	    state(v).value.asInstanceOf[Double] *
	    dx(v).asInstanceOf[Double] / step
	  contended(v) += p.state(v)
	}
      }

      // now put the accumulated variables
      state <<< contended

      // next
      dt += step
    }
  }
}
*/
