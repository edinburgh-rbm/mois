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
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.analysis.differentiation.DerivativeStructure
import org.apache.commons.math3.analysis.differentiation.FiniteDifferencesDifferentiator
import org.apache.commons.math3.analysis.differentiation.GradientFunction
import org.apache.commons.math3.analysis.differentiation.MultivariateDifferentiableFunction
import org.apache.commons.math3.analysis.differentiation.UnivariateDifferentiableFunction
import org.apache.commons.math3.analysis.MultivariateVectorFunction

abstract class HamiltonianProcess(name: String) extends ODE(name) {

  type F = () => Double

  var differentiator = new FiniteDifferencesDifferentiator(3, 0.25)
  private var gradH: GradientFunction = null
  private var energy: F = null

  case class H(q: Seq[DoubleVar], p: Seq[DoubleVar])
       extends MultivariateDifferentiableFunction {
    private val phase = q ++ p
    assert(q.size == p.size)

    private val unis = new Array[UnivariateFunction](phase.size)
    private val HVar = Double("H")

    private case class Uni(f: F, i: Integer)
		 extends UnivariateFunction {
      def value(x: Double): Double = {
	val save = phase(i).value
	phase(i) := x
	val answer = f()
	phase(i) := save
	answer
      }
    }

    def :=(f: => Double) {
      energy = () => f
      for (i <- 0 until phase.size)
        unis(i) = Uni(energy, i)
      gradH = new GradientFunction(this)
      for (v <- phase)
        vars += v
      HVar := energy() // calculate initial energy
    }

    def value(point: Array[Double]): Double = {
      throw new IllegalArgumentException("unimplemented")
    }

    def value(point: Array[DerivativeStructure]): DerivativeStructure = {
      val partials = new Array[Double](point.size)
      for (i <- 0 until point.size) {
	val pval: DerivativeStructure = differentiator.differentiate(unis(i)).value(point(i))
//	println(s"\tvalue:\t\t${pval.getValue}")
//	println(s"\tallderiv:\t${pval.getAllDerivatives.toSeq.toString}")

	partials(i) = pval.getAllDerivatives()(1+i) //PartialDerivative(1)
      }
//      println(s"point:\t\t${point.map(_.getValue).toSeq.toString}")
//      println(s"energy:\t\t${energy()}")
//      println(s"partials:\t${partials.toSeq.toString}")
      HVar := energy()
      new DerivativeStructure(point.size, 1, Seq(HVar.value) ++ partials: _*)
    }
  }



  override def computeDerivatives(time: Double, 
				  qp: Array[Double],
				  dqp: Array[Double]) {
    val dH = gradH.value(qp)
//    println(s"dH:\t\t${dH.toSeq.toString}")
    val nq = qp.size/2
    for (i <- 0 until nq) {
      dqp(i) = dH(nq + i)
      dqp(nq + i) = -dH(i)
    }
//    println(s"dqp:\t\t${dqp.toSeq.toString}")
//    println("")
  }
}

