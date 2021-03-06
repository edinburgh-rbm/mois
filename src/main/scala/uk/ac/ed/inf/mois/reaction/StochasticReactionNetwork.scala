package uk.ac.ed.inf.mois.reaction

import scala.language.implicitConversions
import scala.annotation.tailrec

import collection.mutable
import util.Random
import scala.math.log
import uk.ac.ed.inf.mois.math.Multiset

/** Base trait for all reaction networks that use concentrations of
  * molecules as a measure for species (as opposed to
  * population-based reaction networks).
  */
abstract class StochasticReactionNetwork
    extends PopulationBasedReactionNetwork[Double]
       with KineticCatalyticReactionNetwork[Double, Double] {

  override def stringPrefix = "StochasticReactionNetwork"

  class Reaction(val lhs: Multiset[Species], val rhs: Multiset[Species])
      extends UnratedReaction with CatalysableReaction

  object Reaction extends ReactionFactory {
    def apply(lhs: Multiset[Species], rhs: Multiset[Species]) = new Reaction(lhs, rhs)
  }

  def count(m: Multiset[Species]): Double = {
    for ((s, n) <- m; i <- 0 until n)
    yield 0.0 max (s.value - i)
  }.product

  val rng = Random
  var propensities = Array.empty[Double]
  var time: Double = 0.0

  val rxns = mutable.ArrayBuffer.empty[KineticReaction]
  def reactions(rss: Seq[KineticReaction]*) =
    for (rs <- rss; r <- rs) rxns += r
  implicit def rxnToSeq(r: KineticReaction) = Seq(r)

  def nextReaction(totalPropensity: Double) = {
    val rand = rng.nextDouble * totalPropensity
    var i = 0
    var sum = 0.0
    while (sum < rand) {
      sum += propensities(i)
      i += 1
    }
    rxns(i-1)
  }

  def markovStep {
    for ((rxn, i) <- rxns.zipWithIndex)
      propensities(i) = rxn.rate
    val totalPropensity = propensities.sum
    require(totalPropensity > 0.0, "deadlock")
    val dt = log(1 / rng.nextDouble) / totalPropensity
    time += dt
    val rxn = nextReaction(totalPropensity)
    applyReaction(rxn)
  }

  def applyReaction(rxn: KineticReaction) {
    import spire.implicits._
    for ((s, n) <- rxn.lhs) s -= n
    for ((s, n) <- rxn.rhs) s += n
  }

  override def step(t: Double, dt: Double) {
    if (propensities.size != rxns.size)
      propensities = Array.fill(rxns.size)(0.0)
    time = t
    val finish = t + dt
    while (time <= finish) markovStep
  }
}
