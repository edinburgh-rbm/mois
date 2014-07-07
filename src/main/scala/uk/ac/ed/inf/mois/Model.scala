package uk.ac.ed.inf.mois

/**
 * `MoisMain` is the entry point for command line programs that
 * run models. For how to use this, see the mois-examples repository
 */
abstract class Model(name: String) extends ProcessGroup(name) {

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

  private case class Config(
    val begin: Double = 0.0,
    val end: Double = 50.0, // why is 50.0 the default here?
    val step: Double = 10.0, // and 10.0 here?
    val format: String = "tsv",
    val output: java.io.Writer =
      new java.io.PrintWriter(new java.io.OutputStreamWriter(System.out, "UTF-8")),
    val useFile: Boolean = false
  )

  private val parser = new scopt.OptionParser[Config](name) {
    val p = getClass.getPackage
    val name = p.getImplementationTitle
    val version = p.getImplementationVersion
    head(name, version)

    opt[Double]('b', "begin") action { (x, c) =>
      c.copy(begin = x)
    } text("Simulation start time (default: 0.0)")

    opt[Double]('e', "end") action { (x, c) =>
      c.copy(end = x)
    } text("Simulation end time (default: 50.0)")

    opt[Double]('s', "step") action { (x, c) =>
      c.copy(step = x)
    } text("Simulation (initial) step size")

    opt[String]('i', "initial") action { (filename, c) =>
      val fp = scala.io.Source.fromFile(filename)
      val json = fp.mkString
      fp.close()
      fromJSON(json)
      c
    } text("Initial conditions filename (JSON)")

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
	  addStepHandler(handler)
	  handler.init(cfg.begin, this)
	case _ => throw new IllegalArgumentException(
          "I don't understand format " + cfg.format)
      }

      // run the simulation
      var t = cfg.begin
      while(t < cfg.end) {
	this(t, cfg.step)
	t += cfg.step
      }

      // clean up output
      cfg.output.flush()
      if (cfg.useFile)
        cfg.output.close()

    } getOrElse {
      // some kind of specific error processing? 
    }
  }
}
