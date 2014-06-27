package ed.mois.util

import org.apache.log4j.BasicConfigurator
import org.apache.log4j.{Level, Logger}

object Log {
  var debug = false
  var configured = false
  def setup {
    if (!configured) {
      BasicConfigurator.configure()
      configured = true
    }
    val root = Logger.getRootLogger()
    if (debug)
      root.setLevel(Level.DEBUG)
    else 
      root.setLevel(Level.INFO)
  }
}
