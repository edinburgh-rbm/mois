package uk.ac.ed.inf.mois

import scala.language.implicitConversions

import collection.mutable

/** Base trait for all reaction networks that use concentrations of
  * molecules as a measure for species (as opposed to
  * population-based reaction networks).
  */
abstract class DeterministicReactionNetwork(val name: String)
    extends BaseODE
       with ConcentrationBasedReactionNetwork
       with KineticCatalyticReactionNetwork {

  override def stringPrefix = "DeterministicReactionNetwork"

  private val rxns = mutable.ArrayBuffer.empty[KineticReaction]
  def reactions(rss: Seq[KineticReaction]*) =
    for (rs <- rss; r <- rs) rxns += r
  implicit def rxnToSeq(r: KineticReaction) = Seq(r)

  class Reaction(val lhs: Multiset, val rhs: Multiset)
      extends UnratedReaction with CatalysableReaction

  object Reaction extends ReactionFactory {
    def apply(lhs: Multiset, rhs: Multiset) = new Reaction(lhs, rhs)
  }

  override def step(t: Double, dt: Double) {
    if (vars.size != species.size) {
      funs.clear
      vars.clear
      indices.clear
      // compute derivates
      for ((m, s) <- species)
        addODE(s, ys => (for (rxn <- rxns if rxn(s) != 0) yield
          rxn(s) * rxn.rate).sum)
    }
    super.step(t, dt)
  }
}
