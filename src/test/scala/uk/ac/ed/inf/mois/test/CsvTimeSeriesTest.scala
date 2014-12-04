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
import java.io.{File, FileWriter}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class CsvTimeSeriesTest extends FlatSpec with Matchers with BeforeAndAfter {

  before {
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
    val t = Double("sim:t")
    val x1 = Int("ex:x1")
    val x2 = Double("ex:x2")
  }

  "csv timeseries" should "read in and update at the right time" in {
    val p = new P
    p.init(0)

    def check(t: Double, x1: Int, x2: Double) {
      p.t.value should equal (t)
      p.x1.value should equal (x1)
      p.x2.value should equal (x2)
    }

    check(0, 0, 0)
    p(0, 1)
    check(1, 0, 0)
    p(1, 1)
    check(2, 1, 2)
    p(2, 0.5)
    check(2.5, 2, 4)
    p(2.5, 1)
    check(3.5, 2, 4)
    p(3.5, 1)
    check(4.5, 3, 6)
    p(4.5, 1)
    check(5.5, 3, 6)

    p.reset(0)
    p(2, 1)
    check(3, 2, 4)
  }
}


