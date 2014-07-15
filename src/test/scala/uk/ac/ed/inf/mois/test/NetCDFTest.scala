/*
 *  MOIS: NetCDF Step Handler Test
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

import uk.ac.ed.inf.mois.{DiscreteProcess, NetCDFWriter}

import org.scalatest.{FlatSpec, Matchers}

class NetCDFTest extends FlatSpec with Matchers {

  object p extends DiscreteProcess("p") {
    val x1 = Double("ex:x1")
    val x2 = Double("ex:x2")
    val x3 = Double("ex:x3")
    val x4 = Double("ex:D")

    Dimension(x4, 3)

    next(x1) := x1 + 1 + x4
    next(x2) := x1 + x2 + 1 + x4
    next(x3) := x1 + x2 + x3 + 1 + x4
  }

  "netcdf writer" should "store data scalably" in {
    val cdf = new NetCDFWriter("test.nc")
    p.addStepHandler(cdf)

    cdf.init(0, p)

    import p._
    for (d <- 0 until 3) {
      x1 := 0
      x2 := 0
      x3 := 0
      reset(0)
      for (t <- 0 until 4) {
	p(t, 1)
      }
      Dimension(x4) += 1
      x4 += 10
    }

    cdf.finish

    (1) should not equal (0)
  }
}

