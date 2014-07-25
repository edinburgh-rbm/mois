/*
 *  MOIS: Model Test
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

import org.scalatest.{FlatSpec, Matchers}
import uk.ac.ed.inf.mois.{Process, Model}

class TestModel1 extends Model {
  val a = Double("a")
  a.annotate("long_name", "The a parameter")
  a.annotate("units", "1/u")
  object process extends Process("test model 1") {
    annotate("title", "This is TestModel1")
    val x = Double("x")
    x.annotate("long_name", "The x value")
    x.annotate("units", "u")
    def step(t: Double, tau: Double) {}
  }
}

class TestModel2 extends Model {
  object process extends Process("test model 2") {
    def step(t: Double, tau: Double) {}
  }
}

class ModelTest extends FlatSpec with Matchers {
  "model constructor" should "find a fully qualified model" in {
    val testModel = Model("uk.ac.ed.inf.mois.test.TestModel1")
    assert(testModel != null)
  }

  it should "find a partially qualified model" in {
    val testModel = Model("TestModel1")
    assert(testModel != null)
  }

  it should "not find a nonexistent model" in {
    intercept[IllegalArgumentException] {
      val testModel = Model("nonexistent")
    }
  }

  it should "give an error when asking for ambiguous names" in {
    intercept[IllegalArgumentException] {
      val testModel = Model("TestModel")
    }
  }

  it should "find at least four models when asked for all of them" in {
    (Model.all.size >= 4) should be (true)
  }
}
