/*
 *  MOIS: NetCdf Step Handler Test
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

import uk.ac.ed.inf.mois.{DiscreteProcess, NetCdfWriter}
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._

import java.io.File
import scala.sys.process._
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class NetCdfTest extends FlatSpec with Matchers with BeforeAndAfter {

  after {
    new File("test.nc").delete
  }

  class P extends DiscreteProcess[Double] {
    annotate ("rdfs:label", "NetCDF Test Process")
    val x1 = Double("ex:x1")
    val x2 = Double("ex:x2")
    val x3 = Double("ex:x3")
    val x4 = Double("ex:D") dimension(3)

    next(x1) := x1 + 1 + x4
    next(x2) := x1 + x2 + 1 + x4
    next(x3) := x1 + x2 + x3 + 1 + x4
  }

  val expected = 
"""netcdf test {
dimensions:
    sim\:t = UNLIMITED ; // (4 currently)
    ex\:D = 3 ;
variables:
    double sim\:t(sim\:t) ;
    double ex\:D(ex\:D) ;
    double ex\:x1(sim\:t, ex\:D) ;
    double ex\:x2(sim\:t, ex\:D) ;
    double ex\:x3(sim\:t, ex\:D) ;

// global attributes:
        :rdfs\:label = "NetCDF Test Process" ;
        :class = "uk.ac.ed.inf.mois.test.NetCdfTest$P" ;
data:

 sim\:t = 1, 2, 3, 4 ;

 ex\:D = 0, 10, 20 ;

 ex\:x1 =
  1, 11, 21,
  2, 22, 42,
  3, 33, 63,
  4, 44, 84 ;

 ex\:x2 =
  1, 11, 21,
  3, 33, 63,
  6, 66, 126,
  10, 110, 210 ;

 ex\:x3 =
  1, 11, 21,
  4, 44, 84,
  10, 110, 210,
  20, 220, 420 ;
}
"""

  "netcdf writer" should "store data scalably" in {
    val p = new P
    val cdf = new NetCdfWriter("test.nc")
    p.addStepHandler(cdf)

    p.init(0)

    import p._
    for (d <- 0 until 3) {
      x1 := 0
      x2 := 0
      x3 := 0
      reset(0)
      for (t <- 0 until 4) {
	p(t, 1)
      }

      p.dimension(x4) += 1
      x4 += 10
    }

    cdf.finish

    var got: String = null
    val pb = List("sh", "-c", "ncdump test.nc | sed -e /created/d -e 's/\t/    /g'")
    val pio = new ProcessIO(_ => (),
      stdout => {
        got = scala.io.Source.fromInputStream(stdout).mkString
      },
      _ => ())
    val ncdump = pb.run(pio)

    ncdump.exitValue should equal (0)
    got should equal (expected)
  }
}
