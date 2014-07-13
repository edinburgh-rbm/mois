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

/** A `Process` is basically a `State` and a function that operates
  * upon it parametrised by time.
  */
abstract class BaseProcess extends VarContainer {

  val name: String
  val stepHandlers = mutable.ArrayBuffer.empty[StepHandler]

  def addStepHandler(sh: StepHandler) {
    stepHandlers += sh
  }
  
  def state: Seq[Var[_]] = allVars.values.toSeq

  /** This function takes the state from where it is at
    * time t to where it is at t + tau. This must be supplied
    * by concrete sub-classes.
    */
  def step(t: Double, tau: Double) 

  /** A wrapper around the user defined step function to calculate
    * changes.
    */
  def apply(t: Double, tau: Double) {
    step(t, tau)
    for (sh <- stepHandlers)
      sh.handleStep(t+tau, this)
  }

  def stringPrefix = "BaseProcess"
  override def toString = stringPrefix + "(\"" + name + "\")"
}  

abstract class Process(val name: String)
    extends BaseProcess with VarConversions {
  override def stringPrefix = "Process"
}

