package uk.ac.ed.inf.mois

/**
 * `MoisMain` is the entry point for command line programs that
 * run models. For how to use this, see the mois-examples repository
 */
abstract class MoisMain {
  val process: Process
  val state: State
  val name: String

  case class Config(
    val begin: Double = 0.0,
    val end: Double = 50.0,
    val step: Double = 10.0,
    val format: String = "tsv",
    val output: java.io.Writer =
      new java.io.PrintWriter(new java.io.OutputStreamWriter(System.out, "UTF-8")),
    val outfile: java.io.File = null
  )

  val parser = new scopt.OptionParser[Config](name) {
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
      state <<< State.fromJSON(json)
      c
    } text("Initial conditions filename (JSON)")

    opt[String]('f', "format") action { (format, c) =>
      c.copy(format = format)
    } text("Timeseries output format (default: tsv)")

    opt[String]('o', "output") action { (output, c) =>
      val fp = new java.io.File(output)
      c.copy(output = new java.io.PrintWriter(fp), outfile = fp)
    } text("Output file (default: stdout)")
  }

  def main(args: Array[String]) {
    parser.parse(args, Config()) map { cfg =>
      // set up initial state
      process.state <<< state

      // set up output
      cfg.format match {
	case "tsv" =>
	  val handler = new TsvWriter(cfg.output)
	  process.addStepHandler(handler)
	  handler.init(cfg.begin, process.state)
	case _ =>
      }

      // run the simulation
      var t = cfg.begin
      while(t < cfg.end) {
	process(t, cfg.step)
	t += cfg.step
      }

      // clean up output
      cfg.output.flush
      if (cfg.outfile != null) {
	cfg.output.close
      }

    } getOrElse {
      // some kind of specific error processing? 
    }
  }
}
