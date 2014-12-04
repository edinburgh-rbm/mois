/*
 *  MOIS: Pre-computed Time Series
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

import scala.language.postfixOps
import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}
import java.io.File
import spire.algebra.Rig
import spire.implicits._

abstract class TimeSeries extends Process {
}

class CsvTimeSeries(
  filename: String,
  sep: Char = '\t'
) extends TimeSeries {
  implicit object Format extends DefaultCSVFormat {
    override val delimiter = sep
  }
  var reader = CSVReader.open(new File(filename))
  val header = {
    val row = reader.readNext.get
    row map(i => (i, row indexOf i)) toMap
  }
  type Setter = List[String] => Unit
  var setters: List[Setter] = null

  override def init(t: Double) {
    super.init(t)
    setters = state.getTypes.foldLeft(List.empty[Setter]) { (ss, t) =>
      println(s"hello ${t}")
      ss ++ state.getMeta(t).map { m =>
        println(s"vvv ${m}")
        val v = state.getVar(m)(t)
        val idx = header(m.identifier)
        val fs = fromString(m.rig)
        def set(row: List[String]) {
          println(s"setting ${v} -> ${row(idx)}")
          fs(row(idx))
        }
        set _
      }
    }
    val row = reader.readNext.get
    setters.map(s => s(row))
  }

  override def step(t: Double, tau: Double) {
    val row = reader.readNext.get
    println(s"XXX ${row}")
    setters.map(s => s(row))
  }
}


private[mois] object fromString {
  def apply[T](rig: Rig[T]): String => T = {
    import scala.Predef.augmentString
    if (rig == Rig[Int]) {
      (toInt _).asInstanceOf[String => T]
    } else if (rig == Rig[Byte]) {
      (toByte _).asInstanceOf[String => T]
    } else if (rig == Rig[Long]) {
      (toLong _).asInstanceOf[String => T]
    } else if (rig == Rig[Short]) {
      (toShort _).asInstanceOf[String => T]
    } else if (rig == Rig[Float]) {
      (toFloat _).asInstanceOf[String => T]
    } else if (rig == Rig[Double]) {
      (toDouble _).asInstanceOf[String => T]
    } else {
      // FIXME: warning or something here
      def zero(s: String) = {
        println("ADSADADASASADADSD")
        rig.zero
      }
        (zero _).asInstanceOf[String => T]
    }
  }
  def toInt(s: String) = {
    println("intiyyy")
    augmentString(s).toInt
  }
  def toByte(s: String) = augmentString(s).toByte
  def toLong(s: String) = augmentString(s).toLong
  def toShort(s: String) = augmentString(s).toShort
  def toFloat(s: String) = augmentString(s).toFloat
  def toDouble(s: String) = augmentString(s).toDouble
}
