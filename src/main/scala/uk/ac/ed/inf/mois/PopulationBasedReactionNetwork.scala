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
    def apply(meta: VarMeta) = {
      val s = new PopulationBasedSpecies(meta)
      species += meta -> s
      s
    }
  }
}
