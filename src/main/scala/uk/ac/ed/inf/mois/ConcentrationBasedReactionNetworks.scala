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
