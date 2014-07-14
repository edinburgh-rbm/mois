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

import uk.ac.ed.inf.mois.{ODE, NetCDFWriter}

import org.scalatest.{FlatSpec, Matchers}

class NetCDFTest extends FlatSpec with Matchers {

  object p extends ODE("p") {
    val x1 = Double("ex:x1")
    val x2 = Double("ex:x2")
    val x3 = Double("ex:x3")

    d(x1) := x1 + 1
    d(x2) := 2*x2 - 2*x1
    d(x3) := x3 + 3*x2
  }

  "netcdf writer" should "store data scalably" in {
    val cdf = new NetCDFWriter("test.nc")
    p.addStepHandler(cdf)

    cdf.init(0, p)

    p(1, 1)
    p(2, 1)
    p(3, 1)

    cdf.finish

    (1) should not equal (0)
  }
}


