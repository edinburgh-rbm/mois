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

/** A `Process` is basically a container of mutable variables
  * ([[VarContainer]]) and a function that operates them parametrised
  * by time. It may be [[Process.annotate]]d so that it may
  * be self-documenting and introspectable.
  *
  * A `Process` also has a list of [[StepHandler]]s that run each
  * time step after the computation, their purpose is post-processing
  * and output of the data.
  *
  * It is possible to parametrise by other values in addition
  * to time. For example to run the same simulation several times with
  * different initial conditions. Such variables are called
  * [[Process.Dimension]]s and are of fixed size -- unlike time which
  * has unlimited range.
  */
abstract class Process extends StateBuilder with Annotation {
  lazy val state = buildState

  /** the list of handlers that run after each step */
  val _stepHandlers = mutable.ArrayBuffer.empty[StepHandler]
  lazy val stepHandlers = _stepHandlers.toArray

  /** a list of dimensions and their size/index */
  val dimensions = mutable.Map.empty[VarMeta, Int]

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
    annotate("class", getClass.getName)
  }

  /** This function takes the state from where it is at
    * time t to where it is at t + tau. This must be supplied
    * by concrete sub-classes. Except for testing, it is always
    * called via the [[Process.apply]] method so that any
    * [[StepHandler]]s also get executed.
    *
    * @param t the time at the beginning of the step
    * @param tau the size of the step (delta-t)
    */
  def step(t: Double, tau: Double) { }

  /** A wrapper around the user defined step function to calculate
    * changes. This wrapper is the main way that the step function
    * should be called and it has the additional responsibility of
    * calling any [[StepHandler]]s.
    *
    * @param t the time at the beginning of the step
    * @param tau the size of the step (delta-t)
    */
  def apply(t: Double, tau: Double) = {
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
    initStateIndices(state)
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
    _stepHandlers += sh
  }

  /** Needed for NetCDF et al. TODO: explain better */
  class Dimension[T](i: Index[T]) {
    def apply = dimensions(i.meta)
    def update(x: Int) { dimensions(i.meta) = x }
    def +=(x: Int) { dimensions(i.meta) = dimensions(i.meta) + x }
    def -=(x: Int) { dimensions(i.meta) = dimensions(i.meta) - x }
    def *=(x: Int) { dimensions(i.meta) = dimensions(i.meta) * x }
    def /=(x: Int) { dimensions(i.meta) = dimensions(i.meta) / x }
  }
  object Dimension {
    def apply[T](i: Index[T], size: Int): Dimension[T] = {
      dimensions += i.meta -> size
      Dimension(i)
    }
    def apply[T](i: Index[T]): Dimension[T] = new Dimension(i)
  }

  def stringPrefix = "Process"
  override def toString = stringPrefix
}
