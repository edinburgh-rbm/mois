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
package uk.ac.ed.inf.mois.reaction

import scala.language.implicitConversions

import collection.mutable
import scala.math.pow
import uk.ac.ed.inf.mois.ode.ODEBase
import uk.ac.ed.inf.mois.math.Multiset

/** Base trait for all reaction networks that use concentrations of
  * molecules as a measure for species (as opposed to
  * population-based reaction networks).
  */
abstract class DeterministicReactionNetwork[T, D]
    extends ReactionNetwork[T] with ODEBase[T, D]
       with ConcentrationBasedReactionNetwork[T]
       with KineticCatalyticReactionNetwork[T, D] {
  override def stringPrefix = "DeterministicReactionNetwork"

  val rxns = mutable.ArrayBuffer.empty[KineticReaction]
  def reactions(rss: Seq[KineticReaction]*) =
    for (rs <- rss; r <- rs) rxns += r
  implicit def rxnToSeq(r: KineticReaction) = Seq(r)

  class Reaction(val lhs: Multiset[Species], val rhs: Multiset[Species])
      extends UnratedReaction with CatalysableReaction

  object Reaction extends ReactionFactory {
    def apply(lhs: Multiset[Species], rhs: Multiset[Species]) = new Reaction(lhs, rhs)
  }

  // spire's pow is brain damaged with Jets, this 
  // is a kludge needed for the Rosenbrock...
  @inline private def xpow(a: D, b: Int): D = {
    val b_ = _fromInt(b)
    var i = 0
    var result = _fd.one
    while (i < b) {
      result = _fd.times(a, result)
      i += 1
    }
    result
  }

  def count(m: Multiset[Species]): D = {
    // Optimised version of
    //
    //(for ((s, n) <- m) yield pow(s.value, n)).product
    //
    var c: D = _fd.one
    for ((s, n) <- m) {
//      println(s"XXXXXXX ${s} ${vToD(s)}")
//      c = _fd.times(c, _nr.fpow(vToD(s), _fromInt(n)))
      c = _fd.times(c, xpow(vToD(s), n))
    }
    c
  }

  override def init(t: Double) {
    for (s <- species) {
      // Optimised version of
      //
      // d(s) := (for (rxn <- rxns if rxn(s) != 0) yield
      //          rxn(s) * rxn.rate).sum
      //
      var i:   Int = 0
      var sum: D = _fd.zero
      val reactions = (for (rxn <- rxns if rxn(s) != 0) yield rxn)
      vars += s
      funs += { () =>
//        println(s"calculating.... ${s}")
        i = 0; sum = _fd.zero
        while (i < reactions.size) {
          val r = reactions(i)
//          println(s"... ${sum} + ${r(s)} * ${r.rate}")
          sum = _rg.plus(sum, _fd.times(_fromInt(r(s)), r.rate))
          i += 1
        }
        sum
      }
    }
    super.init(t)
  }
}
