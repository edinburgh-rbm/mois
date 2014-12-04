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
  private var reader = CSVReader.open(new File(filename))
  private val header = {
    val row = reader.readNext.get
    row map(i => (i, row indexOf i)) toMap
  }
  private type Setter = List[String] => Unit
  private var setters: List[Setter] = null

  override def init(t: Double) {
    super.init(t)
    setters = state.getTypes.foldLeft(List.empty[Setter]) { (ss, t) =>
      ss ++ state.getMeta(t).map { m =>
        val v = state.getVar(m)(t)
        val idx = header(m.identifier)
        def set(row: List[String]) {
          v.updateFromString(row(idx))
        }
        set _
      }
    }
  }

  private var prevRow: Option[List[String]] = None
  private var prevTime: Double = scala.Double.NegativeInfinity
  private var nextRow: Option[List[String]] = None
  private var nextTime: Double = scala.Double.NegativeInfinity

  override def step(t: Double, tau: Double) {
    while (t >= nextTime && getRow) {}
    if (t >= prevTime && prevRow.isDefined) {
      val row = prevRow.get
      setters.map(s => s(row))
    }
  }

  private def getRow = {
    if (nextRow.isDefined) {
      prevRow = nextRow
      prevTime = nextTime
    }

    val row = reader.readNext
    if (row.isDefined) {
      nextRow = row
      nextTime = row.get(header("sim:t")).toDouble
    } else {
      nextTime = scala.Double.NaN
    }
    row.isDefined
  }

}
