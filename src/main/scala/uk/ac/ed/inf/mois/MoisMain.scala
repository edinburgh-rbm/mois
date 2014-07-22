/*
 *  MOIS: Main Model Entry-Point and Command-Line Processing
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

/**
 * `MoisMain` is the entry point for command line programs that
 * run models. For how to use this, see the mois-examples repository
 */
object MoisMain {

  // RHZ: I think begin, end and step are part of the definition of a
  // Model and shouldn't be given in the command line.  Why?
  // Because the definition of a Model should contain everything you
  // needed to get the results.  The other three can be given from the
  // command line for the same reason.

  // WW: Because begin is an initial condition just like the x_i. You
  // would typically want to run the model for a shorter time while
  // developing it. In production you might want to run from t_0 to
  // t_1, dump the state, and then run from t_1 to t_2 for various
  // reasons. I kind of agree about step size though, since it can only
  // be set on the top level here anyways.
  //
  // I also think this class should be named back to MoisMain and
  // should not be a ProcessGroup (although it might be a Process
  // or at least a VarContainer) This is because the scheduler-like
  // behaviour does not belong here.

  // RHZ: It surely belongs here no?  You want to be able to set which
  // scheduler you want to use for your model.  In the typical case
  // this will be the only ProcessGroup in the whole model probably.
  // Wrt dumping the state after t_1, that's what the StepHandler is
  // for no?

  // WW: the correct way to do that is to have a "configuration" blob
  // that gets passed to the top process. If it understands about
  // schedulers, its sets up its scheduler. But not all processes have
  // schedulers and there's no reason not to run them individually.
  // The StepHandler should also be set up like this.
  //
  // About dumping and restarting, no that's not what the StepHandler
  // is for. Java is a memory hog. This is a garbage collected language.
  // We are expecting models to be written by academics. It will inevitably
  // happen that a simulation cannot run for the whole duration, so
  // dump and re-read state, and restart is an out for that.

  // RHZ: I agree all this command line stuff doesn't belong in a
  // model. But the idea would be that the user only has to define the
  // `model: Process` and not all the boilerplate `class X extends
  // MoisMain { val model = ... }` but anyway, if you prefer this way
  // it's up to you.
  //
  // I was not saying we should dump and restart, where did you read
  // that?  I was just saying that if the user wants to dump the state
  // after t_1, they can do that using a StepHandler that has that
  // condition.  That is definitely within the scope of StepHandler.

  case class Config(
    val command: Option[String] = None,
    val begin: Option[Double] = Some(0.0),
    val duration: Option[Double] = None,
    val stepHandlers: Seq[Option[StepHandler]] = Seq[Option[StepHandler]](),
    val dumpState: Boolean = false,
    val model: Option[Model] = None,
    val params: Option[String] = None,
    val initial: Option[String] = None
  )

  private val p = getClass.getPackage
  private val name = p.getImplementationTitle

  val optParser = new scopt.OptionParser[Config](name) {
    val version = p.getImplementationVersion
    head(name, version)

    cmd("info") action { (_, c) =>
      c.copy(command = Some("info"))
    } text("Get information on a model") children(
	arg[String]("<model>") action { (modelName, c) =>
	  try {
	    val model = Model(modelName)
	    c.copy(model = Some(model))
	  } catch {
	    case e: IllegalArgumentException => {
	      Console.err.println(e)
	      c
	    }
	  }
	} required () text("Model name"),

	checkConfig { c =>
	  if (c.command == Some("info")) {
	    if (!c.model.isDefined)
	      failure("Could not find the requested model")
	    else
	      success
	  } else {
	    success
	  }
	}
      )
    note("") // spacer

    cmd("list") action { (_, c) =>
      c.copy(command = Some("list"))
    } text("List models")
    note("") // spacer


    cmd("model") action { (_, c) =>
      c.copy(command = Some("run"))
    } text("Run a model") children(
      opt[Double]('d', "duration") action { (x, c) =>
	c.copy(duration = Some(x))
      } required() text("Simulation duration (mandatory)"),

      opt[Double]('b', "begin") action { (x, c) =>
	c.copy(begin = Some(x))
      } text("Simulation start time (default: 0.0)"),
      
      opt[String]('i', "initial") action { (filename, c) =>
	val fp = scala.io.Source.fromFile(filename)
	val json = fp.mkString
	fp.close()
	c.copy(initial = Some(json))
      } text("Initial conditions filename (JSON)"),

      opt[Boolean]('s', "state") action { (x, c) =>
	c.copy(dumpState = true)
      } text("Dump state at end of simulation"),

      opt[String]('o', "output") action { (outspec, c) =>
	c.copy(stepHandlers = c.stepHandlers ++ Seq(getStepHandler(outspec)))
      } optional() unbounded() text("Output specification"),

      arg[String]("<model>") action { (modelName, c) =>
	try {
	  val model = Model(modelName)
	  c.copy(model = Some(model))
	} catch {
	  case e: IllegalArgumentException => {
	    Console.err.println(e)
	    c
	  }
	}
      } required () text("Model name"),

      note(""),
      note("Allowed output specifications:"),
      note("\ttsv:\t\t\tTab-separated values to standard output"),
      note("\ttsv:<filename>\t\tTab-separated values to the given file"),
      note("\tnetcdf:<filename>\tNetCDF output to the given file"),

      checkConfig { c =>
	if (c.command == Some("run")) {
	  if (!c.model.isDefined)
	    failure("Could not find the requested model")
	  else if (!c.stepHandlers.foldLeft(true)((acc, shopt) => acc && shopt.isDefined))
	    failure("Bad step handlers")
	  else 
	    success
	} else {
	  success
	}
      }
    )

    checkConfig { c =>
      if (!c.command.isDefined)
	failure("Must specify a command")
      else
	success
    }
  }

  def getStepHandler(spec: String): Option[StepHandler] = {
    val fmtargs = spec split ":"
    val format = fmtargs(0)
    format match {
      case "tsv" => {
	fmtargs.size match {
	  case 1 => {
	    // ugly monkey patch to allow output to stdout when running
	    // under sbt cli
	    val osw = new java.io.OutputStreamWriter(System.out, "UTF-8")
	    val stdout = new java.io.PrintWriter(osw)
	    Some(new TsvWriter(stdout) {
	      override def finish {}
	    })
	  }
	  case 2 => {
	    val fp = new java.io.File(fmtargs(1))
	    Some(new TsvWriter(new java.io.PrintWriter(fp)))
	  }
	  case _ => {
	    System.err.println("Invalid spec for TSV output")
	    None
	  }
	}
      }
      case "netcdf" => {
	fmtargs.size match {
	  case 2 => Some(new NetCDFWriter(fmtargs(1)))
	  case _ => {
	    System.err.println("Invalid spec for NetCDF output")
	    None
	  }
	}
      }
      case _ => {
	System.err.println("Unknown output format")
	None
      }
    }
  }

  def info(cfg: Config) {
    val model = cfg.model.get
    model.init(0)
    println("model parameters:")
    for (v <- model.allVars.values) {
      println(String.format("    %s", v))
      for ((k, a) <- v.meta.annotations)
	println(String.format("%24s: %s", k, a.toString))
    }

    println("process tree:")
    def prprocess(proc: BaseProcess, prefix: String) {
      for ((k,a) <- model.process.annotations)
	println(String.format("%s%16s: %s", prefix, k, a.toString))
      println(String.format("%s%16s:", prefix, "variables"))
      for (v <- model.process.state) {
	println(String.format("%s          %s", prefix, v))
	for ((k, a) <- v.meta.annotations) 
          println(String.format("%s%32s: %s", prefix, k, a.toString))
      }
    }
    prprocess(model.process, "")
  }

  def list(cfg: Config) {
    println("Known models:\n\t" +
	    Model.all
	      .sortBy(_.toString)
	      .map(_.toString.split("@")(0))
	      .mkString("\n\t"))
  }

  def run(cfg: Config) {
    // get the model
    val model = cfg.model.get
    
    // get simulation start
    val begin = cfg.begin.get
    
    // get duration
    val duration = cfg.duration.get

    // set up output
    for (sh <- cfg.stepHandlers)
      model.process.addStepHandler(sh.get)

    // set initial conditions
    if(cfg.initial.isDefined)
      model.process.fromJSON(cfg.initial.get)
    
    // run the simulation
    model.init(begin)
    model.run(begin, duration)
    model.finish

    // clean up output
    System.out.flush()

    if (cfg.dumpState)
      println(model.process.toJSON)
  }

  def main(args: Array[String]) {
    optParser.parse(args, Config()) map { cfg =>
      cfg.command.get match {
	case "info" => info(cfg)
	case "list" => list(cfg)
	case "run" => run(cfg)
      }
    } getOrElse {
      // some kind of specific error processing? 
    }
  }
}
