/*
 *  MOIS: Variable Constraints
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

class ConstraintViolation(s: String) extends Exception(s)

trait Constraints[T] {
  type Constraint = T => Boolean
  type Bound = T => T
  private val constraints = mutable.ArrayBuffer.empty[Constraint]
  private val bounds = mutable.ArrayBuffer.empty[Bound]
  def addConstraint(c: Constraint) { constraints += c }
  def doCheckConstraints(x: T): Boolean = constraints.forall(_(x))
  def doAssertConstraints(x: T) {
    if (!doCheckConstraints(x))
      throw new ConstraintViolation(s"$this violated constraint by setting $x")
  }
}
