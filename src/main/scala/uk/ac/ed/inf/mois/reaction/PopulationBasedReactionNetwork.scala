/*
 *  MOIS: Population-based Reaction Network
 *  Copyringht (C) 2014 University of Edinburgh School of Informatics
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
package uk.ac.ed.inf.mois.reaction

import scala.reflect.ClassTag
import spire.algebra.Ring
import uk.ac.ed.inf.mois.{Var, VarMeta}

/** Base trait for all reaction networks that use population of
  * molecules as a measure for species (as opposed to
  * concentration-based reaction networks).
  */
trait PopulationBasedReactionNetwork[T] extends ReactionNetwork[T] {

  override def stringPrefix = "PopulationBasedReactionNetwork"

  type Species = PopulationBasedSpecies

  class PopulationBasedSpecies(v: Var[T])(implicit ring: Ring[T])
      extends BaseSpecies(v) {
    type R = Species
  }

  object Species extends SpeciesFactory {
    def apply(ident: String)(implicit ring: Ring[T], ct: ClassTag[T]) = {
      val idx = addVar[T](ident)
      val s = new PopulationBasedSpecies(idx)
      species += s
      s
    }
  }
}
