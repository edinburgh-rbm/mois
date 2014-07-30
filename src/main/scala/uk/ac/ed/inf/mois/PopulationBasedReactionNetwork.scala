/*
 *  MOIS: Population-based Reaction Network
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

/** Base trait for all reaction networks that use population of
  * molecules as a measure for species (as opposed to
  * concentration-based reaction networks).
  */
trait PopulationBasedReactionNetwork extends ReactionNetwork {

  override def stringPrefix = "PopulationBasedReactionNetwork"

  type Base = Int
  type Species = PopulationBasedSpecies

  class PopulationBasedSpecies(val meta: VarMeta) extends BaseSpecies {

    var value = 0
    type R = Species
    override def copy = new PopulationBasedSpecies(meta) := value

    def += (x: Int) = update (value + x)
    def -= (x: Int) = update (value - x)
    def *= (x: Int) = update (value * x)
    def /= (x: Int) = update (value / x)
    def %= (x: Int) = update (value % x)
  }

  object Species extends SpeciesFactory {
    def apply(meta: VarMeta) =
      if (species contains meta) species(meta)
      else {
        val s = new PopulationBasedSpecies(meta)
        species += meta -> s
        s
      }
  }

  val numericBase = implicitly[Numeric[Int]]
}
