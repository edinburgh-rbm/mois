/*
 *  MOIS: Process
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

/** A `BaseProcess` is basically a container of mutable variables
  * ([[VarContainer]]) and a function that operates them parametrised
  * by time.
  *
  * A `BaseProcess` may also be parametrised by other values in addition
  * to time. These are called [[BaseProcess.Dimension]]s and are of
  * fixed size -- unlike time which is unlimited.
  *
  * A `BaseProcess` also has a list of [[StepHandler]]s that run each
  * time step after the computation, their purpose is post-processing
  * and output of the data.
  *
  * A `BaseProcess` may be [[BaseProcess.annotate]]d so that it may
  * be self-documenting and introspectable.
  */
abstract class BaseProcess extends VarContainer with Annotation {

  /** a process is required to have a name (XXX: really? why?) */
  def name: String
  /** the list of handlers that run after each step */
  val stepHandlers = mutable.ArrayBuffer.empty[StepHandler]
  /** a list of dimensions and their size/index */
  val dimensions = mutable.Map.empty[Var[_], Int]

  /** Automatically annotate the process with its software name and version */
  protected def addBasicAnnotations = {
    // XXX why does this not work?  Implementation title and version
    // always return null for derived classes
    val pkg = getClass.getPackage
    val pkgname = pkg.getImplementationTitle
    val pkgversion = pkg.getImplementationVersion
    if (pkgname != null && pkgversion != null) {
      annotate(pkgname, pkgversion)
    }

    import uk.ac.ed.inf.{mois => m}
    if (m.name != null && m.version != null) {
      annotate(m.name, m.version)
    }

    annotate("name", name)
    annotate("type", stringPrefix)
    annotate("class", getClass.getName)
  }

  /** All variables defined in this `BaseProcess`. */
  def state: Seq[Var[_]] = allVars.values.toSeq

  /** This function takes the state from where it is at
    * time t to where it is at t + tau. This must be supplied
    * by concrete sub-classes. Except for testing, it is always
    * called via the [[BaseProcess.apply]] method so that any
    * [[StepHandler]]s also get executed.
    *
    * @param t the time at the beginning of the step
    * @param tau the size of the step (delta-t)
    */
  def step(t: Double, tau: Double)

  /** A wrapper around the user defined step function to calculate
    * changes. This wrapper is the main way that the step function
    * should be called and it has the additional responsibility of
    * calling any [[StepHandler]]s.
    *
    * @param t the time at the beginning of the step
    * @param tau the size of the step (delta-t)
    */
  def apply(t: Double, tau: Double) {
    step(t, tau)
    for (sh <- stepHandlers)
      sh.handleStep(t+tau, this)
  }

  /**
    * Initialisation hook. Expected to be called once before the process
    * runs for the first time. Initialises step handlers and adds
    * basic annotations. Remember to call `super.init(t)` if you override
    * this!
    *
    * @param t the time at the beginning of the simulation
    */
  def init(t: Double) {
    addBasicAnnotations
    for (sh <- stepHandlers)
      sh.init(t, this)
  }

  /**
    * Reset hook. May be called as necessary. Resets the step handlers.
    * May be overridden in sub-classes to reset internal state. Rember
    * to call `super.reset(t)` if you do this!
    *
    * @param t the new time
    */
  def reset(t: Double) {
    for (sh <- stepHandlers)
      sh.reset(t, this)
  }

  /** Finish hook. Expected to be called once at the end of theprocess.
    * Finishes the step handlers. Remember to call `super.finish` if you
    * override this!
    */
  def finish {
    for (sh <- stepHandlers)
      sh.finish
  }

  /** Add a [[StepHandler]] to the list
    *
    * @param sh the step handler, evidently
    */
  def addStepHandler(sh: StepHandler) {
    stepHandlers += sh
  }

  /** Needed for NetCDF et al. TODO: explain better */
  class Dimension(v: Var[_]) {
    def apply = dimensions(v)
    def update(x: Int) { dimensions(v) = x }
    def +=(x: Int) { dimensions(v) = dimensions(v) + x }
    def -=(x: Int) { dimensions(v) = dimensions(v) - x }
    def *=(x: Int) { dimensions(v) = dimensions(v) * x }
    def /=(x: Int) { dimensions(v) = dimensions(v) / x }
  }
  object Dimension {
    def apply(v: Var[_], size: Int): Dimension = {
      dimensions += allVars(v.meta) -> size
      Dimension(v)
    }
    def apply(v: Var[_]): Dimension = new Dimension(v)
  }

  def stringPrefix = "BaseProcess"
  override def toString = stringPrefix + "(" + name + ")"
}

abstract class Process(val name: String)
    extends BaseProcess with VarConversions {
  override def stringPrefix = "Process"
}
