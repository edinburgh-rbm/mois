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

import scala.collection.mutable
import scala.util.matching.Regex

abstract class Model extends ArrayBackedStateBuilder with Annotation {

  val process: Process
  

  def init(t: Double) {
    merge(process)
    val state = buildState
    initState(state)
    process.init(t)
    // initialise the state with model overrides and initial
    // conditions
    process.state.copyFromAll(state)
  }

  def run(t: Double, tau: Double, n: Int) {
    var i = 0
    val dt = tau/n
    while (i < n) {
      process(t + dt*i, dt)
      i += 1
    }
  }

  def finish {
    process.finish
  }

  def reset(t: Double) {
    process.reset(t)
  }

  /** Convenience method to add a step handler onto the process.
    * see [[Process.addStepHandler]] */
  def addStepHandler(sh: StepHandler) {
    process.addStepHandler(sh)
  }

  /** convenience method to set/override the scheduler iff the process is a group */
  def setScheduler(sched: Scheduler) {
    require(process.isInstanceOf[ProcessGroup], "process must be a group to have a scheduler")
    process.asInstanceOf[ProcessGroup].scheduler = sched
  }

  /** manipulate dimensions -- should be in syntax !? */
  def dimension[T](v: Var[T]) = new Dimension(v)

  /** set a model parameter */
  // def setParam(name: String, value: Double) {
  //   doubleVars(VarMeta(name)) := value
  // }

  /** set a variable (for initial conditions */
  // def setVar(name: String, value: Double) {
  //   process.doubleVars(VarMeta(name)) := value
  // }
}

object Model {
  import scala.reflect._
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.currentMirror
  import scala.collection.JavaConverters._
  import scala.language.implicitConversions
  import java.util.ServiceLoader

  type N = String

  implicit def modelName(m: Model): String = {
    val im = currentMirror reflect m
    im.symbol.toType.toString.split("@")(0)
  }

  def all: Seq[Model] = {
    val models = mutable.ArrayBuffer.empty[Model]
    val need = typeOf[Model]
    for (model <- (ServiceLoader load classOf[Model]).asScala) {
      val im = currentMirror reflect model
      val typ = im.symbol.toType
      if (typ weak_<:< need) {
          models += model
      }
    }
    models.toSeq
  }

  def apply(name: String): Model = {
    val models = all filter(name.r.findFirstIn(_).isDefined)
    models.size match {
      case 0 => throw new IllegalArgumentException(s"no such model $name")
      case 1 => models(0)
      case _ => throw new IllegalArgumentException(s"$name is ambiguous, found\n\t" +
                                                   models.mkString("\n\t"))
    }
  }
}
