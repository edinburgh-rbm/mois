package uk.ac.ed.inf.mois

/** Base trait for all reaction networks that use concentrations of
  * molecules as a measure for species (as opposed to
  * population-based reaction networks).
  */
trait ConcentrationBasedReactionNetwork extends ReactionNetwork {

  override def stringPrefix = "ConcentrationBasedReactionNetwork"

  type Base = Double
  type Specie = ConcentrationBasedSpecie

  class ConcentrationBasedSpecie(val meta: VarMeta)
      extends SpecieIntf with DoubleVarIntf {
    type R = Specie
    override def copy = new ConcentrationBasedSpecie(meta) := value
  }

  object Specie extends SpecieFactory {
    def apply(meta: VarMeta) = {
      val s = new ConcentrationBasedSpecie(meta)
      species += meta -> s
      s
    }
  }
}
