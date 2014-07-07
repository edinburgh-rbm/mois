package uk.ac.ed.inf.mois

/**
 * `MoisMain` is the entry point for command line programs that
 * run models. For how to use this, see the mois-examples repository
 */
abstract class MoisMain(name: String) {

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
  val model: Process

  private case class Config(
    val begin: Double = 0.0,
    val duration: Double = 50.0, // why is 50.0 the default here?
    val format: String = "tsv",
    val output: java.io.Writer =
      new java.io.PrintWriter(new java.io.OutputStreamWriter(System.out, "UTF-8")),
    val useFile: Boolean = false,
    val dumpState: Boolean = false
  )

  private val parser = new scopt.OptionParser[Config](name) {
    val p = getClass.getPackage
    val name = p.getImplementationTitle
    val version = p.getImplementationVersion
    head(name, version)

    opt[Double]('b', "begin") action { (x, c) =>
      c.copy(begin = x)
    } text("Simulation start time (default: 0.0)")

    opt[Double]('d', "duration") action { (x, c) =>
      c.copy(duration = x)
    } text("Simulation duration (default: 50.0)")

    opt[String]('i', "initial") action { (filename, c) =>
      val fp = scala.io.Source.fromFile(filename)
      val json = fp.mkString
      fp.close()
      model.fromJSON(json)
      c
    } text("Initial conditions filename (JSON)")

    opt[Boolean]('s', "state") action { (x, c) =>
      c.copy(dumpState = true)
    } text("Dump state at end of simulation")

    opt[String]('f', "format") action { (format, c) =>
      c.copy(format = format)
    } text("Timeseries output format (default: tsv)")

    opt[String]('o', "output") action { (output, c) =>
      val fp = new java.io.File(output)
      c.copy(output = new java.io.PrintWriter(fp), useFile = true)
    } text("Output file (default: stdout)")
  }

  def main(args: Array[String]) {
    parser.parse(args, Config()) map { cfg =>
      // set up output
      cfg.format match {
	case "tsv" =>
	  val handler = new TsvWriter(cfg.output)
	  model.addStepHandler(handler)
	  handler.init(cfg.begin, model)
	case _ => throw new IllegalArgumentException(
          "I don't understand format " + cfg.format)
      }

      // run the simulation
      model(cfg.begin, cfg.duration)

      // clean up output
      cfg.output.flush()
      if (cfg.useFile)
        cfg.output.close()

      if (cfg.dumpState)
	println(model.toJSON)
    } getOrElse {
      // some kind of specific error processing? 
    }
  }
}
