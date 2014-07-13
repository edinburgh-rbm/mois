package uk.ac.ed.inf.mois

import collection.mutable

/** Base trait for all reaction networks that use concentrations of
  * molecules as a measure for species (as opposed to
  * population-based reaction networks).
  */
abstract class DeterministicReactionNetwork(val name: String)
    extends ODEIntf with ConcentrationBasedReactionNetwork {

  override def stringPrefix = "DeterministicReactionNetwork"

  private val rxns = mutable.ArrayBuffer.empty[Reaction]

  def reactions(rs: Reaction*) = for (r <- rs) rxns += r

  class Reaction(val lhs: Multiset, val rhs: Multiset, var rate: Double)
      extends ReactionIntf {

    override def toString =
      "Reaction(" + lhs + ", " + rhs + ", " + rate + ")"

    def withRate(d: Double) = at(d)
    def atRate(d: Double) = at(d)
    def at(d: Double) = { rate = d; this }
  }

  object Reaction extends ReactionFactory {
    def apply(lhs: Multiset, rhs: Multiset) = Reaction(lhs, rhs, 1.0)
    def apply(lhs: Multiset, rhs: Multiset, rate: Double) =
      new Reaction(lhs, rhs, rate)
  }

  override def step(t: Double, dt: Double) {
    if (vars.size != species.size) {
      funs.clear
      vars.clear
      indices.clear
      // compute derivates
      import scala.math.pow
      for ((m, s) <- species) {
        val stoich = {
          for (rxn <- rxns) yield
            (rxn, rxn.rhs.getOrElse(s, 0) - rxn.lhs.getOrElse(s, 0))
        }.toMap
        val f: Array[Double] => Double = ys =>
          (for (rxn <- rxns if stoich(rxn) != 0) yield
            stoich(rxn) * rxn.rate * (for ((l, n) <- rxn.lhs) yield
              pow(eval(l, ys), n)).product).sum
        // print equation
        // println("d(" + s.meta + ") := " +
        //   (for (rxn <- rxns if stoich(rxn) != 0) yield
        //     stoich(rxn) + " * " + rxn.rate + " * " +
        //     (for ((l, n) <- rxn.lhs) yield
        //       l.meta + (if (n > 1) "^" + n else "")).mkString(" * ")
        //   ).mkString(" + "))
        // add derivative
        addODE(s, f)
      }
    }
    super.step(t, dt)
  }
}
