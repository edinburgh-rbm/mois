/*
 *  MOIS: Hamiltonian Process
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

import scala.collection.mutable
import org.apache.commons.math3.analysis.differentiation.DerivativeStructure
import org.apache.commons.math3.analysis.differentiation.GradientFunction
import org.apache.commons.math3.analysis.differentiation.MultivariateDifferentiableFunction
import org.apache.commons.math3.analysis.MultivariateVectorFunction

abstract class HamiltonianProcess(name: String) extends OrdinaryProcess(name) {
  type F = () => Double

  var gradH: MultivariateVectorFunction = null

  case class H(q: Seq[DoubleVar], p: Seq[DoubleVar])
       extends MultivariateDifferentiableFunction {
    var _f: F = null

    assert(q.size == p.size)

    def :=(f: => Double) {
      _f = () => f
      gradH = new GradientFunction(this)
      for (v <- q ++ p)
        vars += v
    }

    def value(point: Array[Double]): Double = {
      // not very efficient... find a better way
      val phase = (q ++ p)
      val saved = phase map(_.copy)

      assert(phase.size == point.size)
      for (i <- 0 until phase.size)
	phase(i) := point(i)

      val answer = _f()

      // not very efficient... find a better way
      for ((s, v) <- saved zip phase)
        v := s

      answer
    }
    def value(point: Array[DerivativeStructure]): DerivativeStructure = {
      new DerivativeStructure(point.size, 1, 0, value(point map(_.getValue)))
    }
  }

  override def computeDerivatives(time: Double, 
				  qp: Array[Double],
				  dqp: Array[Double]) {
    val dH = gradH.value(qp)
    val nq = qp.size/2
    for (i <- 0 until nq) {
      dqp(i) = dH(nq + i)
      dqp(nq + i) = -dH(i)
    }
  }
}

