/*
 *  MOIS: Python Process
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

import scala.language.dynamics
import scala.collection.mutable
import uk.ac.ed.inf.mois.implicits._

import org.python.util.PythonInterpreter
import org.python.core.{PyException, PyFloat, PyObject}

abstract class PythonProcess extends Process {

  type F = (Double, Double, Seq[Var[Double]]) => Unit
  private val pyFuncs = mutable.ArrayBuffer.empty[(F, Seq[Var[Double]])]

  protected case class py(val vs: Var[Double]*) {
    def := (f: F) = {
      pyFuncs += (f -> vs)
    }
  }

  case class Python(module: String) extends Dynamic {
    private val interp = new PythonInterpreter
    def applyDynamic(func: String)(args: Var[Double]*) = {
      try {
        interp.exec(s"from $module import $func")
      } catch {
        case e: PyException =>
          throw new IllegalArgumentException(e.toString)
      }

      val fh = interp.get(func)
      if (fh == null)
        throw new IllegalArgumentException(s"no such python function ${module}.${func}")

      def wrapper(t: Double, tau: Double, vs: Var[Double]*) {
        val pyArgs =
        (Seq(t, tau).map(new PyFloat(_)) ++ args.map(new PyFloat(_)))
          .toArray
          .asInstanceOf[Array[PyObject]]
        try {
          val pyResult = fh.__call__(pyArgs)
          if (pyResult.isSequenceType) {
            if (pyResult.__len__ != vs.length)
              throw new IllegalArgumentException(s"expected ${module}.${func} to return " +
                                                 s"${vs.length} items and got " +
                                                 s"${pyResult.__len__}")
            for (i <- 0 until vs.length) {
              vs(i) := pyResult.__getitem__(i).asDouble
            }
          } else {
            vs.length match {
              case 0 =>
              case 1 => vs(0) := pyResult.asDouble
              case _ =>
                throw new IllegalArgumentException(s"${module}.${func} didn't return a " +
                                                   s"sequence and ${vs.length} values required")
            }
          }
        } catch {
          case e: PyException =>
            throw new IllegalArgumentException(e.toString)
        }
      }
      wrapper _
    }
  }

  override def step(t: Double, tau: Double) {
    for ((f, vs) <- pyFuncs) {
      f(t, tau, vs)
    }
  }
}
