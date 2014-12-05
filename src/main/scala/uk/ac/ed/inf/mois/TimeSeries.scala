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
import scala.reflect.ClassTag
import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}
import java.io.File
import spire.algebra.{Rig, Order}
import spire.math.Number
import spire.implicits._

abstract class TimeSeries[T : ClassTag : Order](
  time: String = "sim:t"
)(implicit rig: Rig[T]) extends Process {
  // make sure we have a handle for ``time''
  protected[mois] val __t = addVar[T](time)
}

class CsvTimeSeries[T : ClassTag : Order](
  filename: String,
  sep: Char = '\t',
  time: String = "sim:t"
)(implicit rig: Rig[T]) extends TimeSeries[T](time) {
  implicit object Format extends DefaultCSVFormat {
    override val delimiter = sep
  }
  private var reader: CSVReader = null
  private lazy val header = {
    val row = reader.readNext.get
    row map(i => (i, row indexOf i)) toMap
  }
  private type Setter = List[String] => Unit
  private var setters: List[Setter] = null

  private val (tMin, tMax, tNaN) = typeExtrema[T](rig)
  private var prevRow: Option[List[String]] = None
  private var prevTime = tMin
  private var nextRow: Option[List[String]] = None
  private var nextTime = tMin

  override def init(t: Double) {
    super.init(t)
    initReader
    setters = state.getTypes.foldLeft(List.empty[Setter]) { (ss, t) =>
      ss ++ state.getMeta(t)
        .filter { m => header.contains(m.identifier) }
        .map { m =>
          val v = state.getVar(m)(t)
          val idx = header(m.identifier)
          def set(row: List[String]) {
            v.updateFromString(row(idx))
          }
          set _
        }
    }
  }

  override def reset(t: Double) {
    super.reset(t)
    initReader
  }

  private def initReader {
    val eatHeader = if (reader != null) {
      reader.close
      true
    } else {
      false
    }
    reader = CSVReader.open(new File(filename))

    prevRow = None
    prevTime = tMin
    nextRow = None
    nextTime = tMin

    if (eatHeader)
      reader.readNext
  }

  override def step(t: Double, tau: Double) {
    if (prevTime > __t.value) reset(0)
    while (nextTime <= __t.value && getRow) {}
    if (prevTime <= __t.value && prevRow.isDefined) {
      val row = prevRow.get
      setters.map(s => s(row))
    }
  }

  private def getRow = {
    if (nextRow.isDefined) {
      prevRow = nextRow
      prevTime = nextTime
    }

    nextRow = reader.readNext
    if (nextRow.isDefined) {
      nextTime = __t.fromString(nextRow.get(header(time)))
    } else {
      nextTime = tNaN
    }
    nextRow.isDefined
  }
}
