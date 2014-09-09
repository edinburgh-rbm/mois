/*
 *  MOIS: State Test
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

import scala.collection.mutable

import spire.algebra.Rig
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._
import uk.ac.ed.inf.mois.{State, StateBuilder, VarMeta}

class StateTest extends FlatSpec with Matchers {

  val i = implicitly[Rig[Int]]
  val d = implicitly[Rig[Double]]
  val vs = mutable.Map[Rig[_], Array[_]](
    i -> Array(0, 1),
    d -> Array(2.0, 3.0))
  val ms = mutable.Map[Rig[_], Array[VarMeta]](
    i -> Array(VarMeta("x", i), VarMeta("y", i)),
    d -> Array(VarMeta("z", d), VarMeta("w", d)))

  val s = new State(ms, vs)

  "State" should "give metas and vars when asked" in {
    s.getMeta[Int] should be (ms(i))
    s.getMeta[Double] should be (ms(d))
    s.get[Int] should be (vs(i))
    s.get[Double] should be (vs(d))
  }

  it should "create indices that can access their value" in {
    val x = s.getVar[Int](VarMeta("x", i))
    val y = s.getVar[Int](VarMeta("y", i))
    val z = s.getVar[Double](VarMeta("z", d))
    val w = s.getVar[Double](VarMeta("w", d))
    x.value should be (0)
    y.value should be (1)
    z.value should be (2.0)
    w.value should be (3.0)
  }

  val x = s.getVar[Int](VarMeta("x", i))
  val y = s.getVar[Int](VarMeta("y", i))
  val z = s.getVar[Double](VarMeta("z", d))
  val w = s.getVar[Double](VarMeta("w", d))

  it should "create indices that can modify their value" in {
    x := 4
    x.value should be (4)
    (x+y) should be (5)
    y += 4
    y.value should be (5)
    z *= 2
    z.value should be (4.0)
    (z*w) should be (12.0)
  }

  it should "be updatable" in {
    s := Array(6, 7)
    x.value should be (6)
    y.value should be (7)
    // s := Array(true) should throw something
  }

  // -- StateBuilders --

  "a StateBuilder" should "create a State" in {
    val sb = new StateBuilder { }
    val x = sb.addVar[Int]("x")
    val s = sb.buildState
    println(s.meta(i)(0).rig)
    val m = VarMeta("x", i)
    s.meta(i)(0) should be (m)
    s.meta(i) should be (Array(m))
    s.meta.head._1 should be (i)
    s.meta.head._2 should be (Array(m))
    // RHZ: Array equality doesn't work...  It works in the last line
    // because ScalaTest probably calls sameElements instead of equals
    // s.meta.head should equal ((i, Array(m)))
    // s.meta should be (Map(i -> Array(VarMeta("x", i))))
    // s.vars should be (Map(i -> Array(0)))
    // x.value should throw NullPointerException
    sb.initStateIndices(s)
    x.value should be (0)
  }

  class DummyState extends StateBuilder {
    val x = Int("x")
    val y = Int("y")
    val u = Double("u")
    val v = Double("v")
    val b = Boolean("b")
    val state = buildState
    initStateIndices(state)
  }

  it should "support adding and retrieving variables" in {
    val s = new DummyState
    s.x.value should equal (0)
    s.y.value should equal (0)
    s.x := 2
    s.y := 3
    s.x+s.y should equal(5)
  }

  it should "support retrieving all variables of a given type" in {
    val s = new DummyState
    val ints = s.state.get[Int]
    val doubles = s.state.get[Double]
    ints should be (Array(0, 0))
    doubles should be (Array(0.0, 0.0))
  }
}
