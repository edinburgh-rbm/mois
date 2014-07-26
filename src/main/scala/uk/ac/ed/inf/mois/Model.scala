/*
 *  MOIS: Main Model Entry-Point and Command-Line Processing
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
import scala.util.matching.Regex

abstract class Model extends VarContainer with VarConversions {

  val process: BaseProcess

  def init(t: Double) {
    process.init(t)
  }

  def run(t: Double, tau: Double) {
    process(t, tau)
  }

  def finish {
    process.finish
  }

  def reset(t: Double) {
    process.reset(t)
  }
}

object Model {
  import scala.reflect._
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.currentMirror
  import scala.collection.JavaConverters._
  import scala.language.implicitConversions
  import java.util.ServiceLoader

  type N = String

  implicit def modelName(m: Model): String = {
    val im = currentMirror reflect m
    im.symbol.toType.toString.split("@")(0)
  }

  def all: Seq[Model] = {
    val models = mutable.ArrayBuffer.empty[Model]
    val need = typeOf[Model]
    for (model <- (ServiceLoader load classOf[Model]).asScala) {
      val im = currentMirror reflect model
      val typ = im.symbol.toType
      if (typ weak_<:< need) {
          models += model
      }
    }
    models.toSeq
  }

  def apply(name: String): Model = {
    val models = all filter(name.r.findFirstIn(_).isDefined)
    models.size match {
      case 0 => throw new IllegalArgumentException(s"no such model $name")
      case 1 => models(0)
      case _ => throw new IllegalArgumentException(s"$name is ambiguous, found\n\t" +
                                                   models.mkString("\n\t"))
    }
  }
}
