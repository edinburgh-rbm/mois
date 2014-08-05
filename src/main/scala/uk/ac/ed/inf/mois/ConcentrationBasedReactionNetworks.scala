/*
 *  MOIS: Concentration-based Reaction Network
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

/** Base trait for all reaction networks that use concentrations of
  * molecules as a measure for species (as opposed to
  * population-based reaction networks).
  */
trait ConcentrationBasedReactionNetwork extends ReactionNetwork {

  override def stringPrefix = "ConcentrationBasedReactionNetwork"

  type Base = Double
  type Species = ConcentrationBasedSpecies

  class ConcentrationBasedSpecies(val meta: VarMeta)
      extends BaseSpecies with DoubleVarIntf {
    type R = Species
    override def copy = new ConcentrationBasedSpecies(meta) := value
  }

  object Species extends SpeciesFactory {
    def apply(meta: VarMeta) =
      if (species contains meta) species(meta)
      else {
        val s = new ConcentrationBasedSpecies(meta)
        species += meta -> s
        s
      }
  }
}
