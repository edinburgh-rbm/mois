/*
 *  MOIS: Deterministic Reaction Network
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

import collection.mutable
import scala.math.pow

/** Base trait for all reaction networks that use concentrations of
  * molecules as a measure for species (as opposed to
  * population-based reaction networks).
  */
abstract class DeterministicReactionNetwork(val name: String)
    extends BaseODE
       with ConcentrationBasedReactionNetwork
       with KineticCatalyticReactionNetwork {

  override def stringPrefix = "DeterministicReactionNetwork"

  val rxns = mutable.ArrayBuffer.empty[KineticReaction]
  def reactions(rss: Seq[KineticReaction]*) =
    for (rs <- rss; r <- rs) rxns += r
  implicit def rxnToSeq(r: KineticReaction) = Seq(r)

  class Reaction(val lhs: Multiset, val rhs: Multiset)
      extends UnratedReaction with CatalysableReaction

  object Reaction extends ReactionFactory {
    def apply(lhs: Multiset, rhs: Multiset) = new Reaction(lhs, rhs)
  }

  implicit val count: Multiset => Double = m => (
    for ((s, n) <- m) yield pow(s, n)).product

  override def step(t: Double, dt: Double) {
    if (vars.size != species.size) {
      funs.clear
      vars.clear
      // compute derivates
      for ((m, s) <- species)
        d(s) := (for (rxn <- rxns if rxn(s) != 0) yield
          rxn(s) * rxn.rate).sum
    }
    super.step(t, dt)
  }
}
