/*
 *  MOIS: Step Handler Test
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

import uk.ac.ed.inf.mois.{Accumulator, Process, StepHandler, TsvWriter}
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._
import org.scalatest.{FlatSpec, Matchers}

class StepHandlerTest extends FlatSpec with Matchers {

  class P extends Process {
    val x1 = Int("ex:x1")
    override def step(t: Double, tau: Double) {
      x1 += 1
    }
  }

  "accumulator" should "accumulate state" in {
    val acc = new Accumulator
    val p = new P
    p.addStepHandler(acc)
    p.init(0)

    p(0, 1)
    p(1, 1)
    p(2, 1)

    acc(0.0)(p.x1).value should be (0)
    acc(1.0)(p.x1).value should be (1)
    acc(2.0)(p.x1).value should be (2)
    acc(3.0)(p.x1).value should be (3)
  }
}

class TsvWriterTest extends FlatSpec with Matchers {

  class P extends Process {
    // purposely define them in the "wrong" order
    val x2 = Int("ex:x2")
    val x1 = Int("ex:x1")
    override def step(t: Double, tau: Double) {
      x1 += 1
      x2 := 2*x1
    }
  }

  "file output" should "write tsv" in {
    val buffer = new java.io.StringWriter
    val fout = new TsvWriter(buffer)
    val p = new P
    p.addStepHandler(fout)
    p.init(0)

    p(0, 1)
    p(1, 1)
    p(2, 1)

    val expected =
"""t	ex:x1	ex:x2
0.0	0	0
1.0	1	2
2.0	2	4
3.0	3	6
"""
    buffer.toString should be (expected)
  }
}
