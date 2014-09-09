/*
 *  MOIS: Implicit Conversions
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

import scala.language.implicitConversions
import spire.algebra.{Rig, Ring, Field}

object implicits {
  implicit def varValue[T](v: Var[T]) = v.value
  implicit def rigSyntax[T: Rig](v: Var[T]) = new RigVarSyntax[T](v)
  implicit def ringSyntax[T: Ring](v: Var[T]) = new RingVarSyntax[T](v)
  implicit def fieldSyntax[T: Field](v: Var[T]) = new FieldVarSyntax[T](v)
}
