/*
 *  MOIS: CSV TimeSeries Step Handler Test
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
package uk.ac.ed.inf.mois.test

import uk.ac.ed.inf.mois.{CsvTimeSeries}
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._

import java.io.{File, FileWriter}
import com.github.tototoshi.csv.{DefaultCSVFormat}

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class CsvTimeSeriesTest extends FlatSpec with Matchers with BeforeAndAfter {

  before {
    implicit object TSV extends DefaultCSVFormat {
      override val delimiter = '\t'
    }
    val fp = new FileWriter(new File("test.tsv"))
    fp.write(
"""sim:t	ex:x1	ex:x2
0.0	0	0
1.0	1	2
2.0	2	4
3.0	3	6
""")
    fp.close
  }

  after {
    new File("test.tsv").delete
  }

  class P extends CsvTimeSeries("test.tsv") {
    val x1 = Int("ex:x1")
    val x2 = Double("ex:x2")
  }

  "csv timeseries" should "blah" in {
    val p = new P
    p.init(0)
    println(p.header)
    println(p.setters.toList)
    println(s"x1 ${p.x1} x2 ${p.x2}")
    p(0, 1)
    println(s"x1 ${p.x1} x2 ${p.x2}")
    (0) should equal (0)
  }
}


