package uk.ac.ed.inf.mois

/** Base trait for all reaction networks that use population of
  * molecules as a measure for species (as opposed to
  * concentration-based reaction networks).
  */
trait PopulationBasedReactionNetwork extends ReactionNetwork {

  override def stringPrefix = "PopulationBasedReactionNetwork"

  type Base = Int
  type Specie = PopulationBasedSpecie

  class PopulationBasedSpecie(val meta: VarMeta) extends BaseSpecie {

    var value = 0
    type R = Specie
    override def copy = new PopulationBasedSpecie(meta) := value

    def += (x: Int) = update (value + x)
    def -= (x: Int) = update (value - x)
    def *= (x: Int) = update (value * x)
    def /= (x: Int) = update (value / x)
    def %= (x: Int) = update (value % x)
  }

  object Specie extends SpecieFactory {
    def apply(meta: VarMeta) = {
      val s = new PopulationBasedSpecie(meta)
      species += meta -> s
      s
    }
  }
}
