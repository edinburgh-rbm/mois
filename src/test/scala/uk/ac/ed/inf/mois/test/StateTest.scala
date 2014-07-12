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

/*
import org.scalatest.{FlatSpec, Matchers}

import uk.ac.ed.inf.mois.{Var, State, BooleanVar}

class StateTest extends FlatSpec with Matchers {

  "a state" should "support adding and retrieving variables" in {
    val s = new State
    val r1 = Var(1, "ex:r1")
    val r2 = Var(2, "ex:r2")

    // adding a resource to state is done like this
    s += r1
    s += r2

    // RHZ: If State extends Map, we could write this like
    // `s should contain r1` and maybe that would give us a more
    // descriptive error in case the test fails (not sure).
    assert(s contains r1)
    assert(s contains r1.key)
    assert(s contains r2)
    assert(s contains r2.key)
    s(r1) should be (r1)
    s(r2) should be (r2)
    s(r1.key) should be (r1)
    s(r2.key) should be (r2)
  }

  it should "not return variables that aren't there" in {
    val s = new State
    val r3 = Var(3, "ex:r3")
    intercept[NoSuchElementException] {
      s(r3)
    }
    intercept[NoSuchElementException] {
      s(r3.key)
    }
  }

  it should "support arithmetic on state variables" in {
    var s = new State
    val r1 = Var(1, "ex:r1")
    val r2 = Var(2, "ex:r2")

    s += r1
    s += r2

    r1 += 2 + r2
    r1.value should be (5)
    s(r1).value should be (5)

    s(r1) := 0
    r1.value should be (0)
    s(r1).value should be (0)

    // when we change the variable itself
    // it should change the variable via the state
    r1 := 5
    r1.value should be (5)
    s(r1).value should be (5)
  }

  it should "update but not replace a state variable with :=" in {
    var s = new State
    val a = Var(1, "ex:_")
    val b = Var(2, "ex:_")

    s += a
    s := b

    assert(s(a) eq a)
    assert(s(b) eq a)
    assert(!(s(a) eq b))
    a.value should be (2)
  }

  "two states" should "update with <<<" in {
    val s1 = new State
    val s2 = new State

    val r1 = Var(1, "ex:r1")
    val r2 = Var(2, "ex:r2")

    s1 += r1
    s2 += r2

    // update state from other is done like this
    s1 <<< s2

    // assert(s1(r2).value.asInstanceOf[Int] == r2.value)
    s1(r2).value should be (r2.value)
    assert(!(s1(r2) eq r2))
  }

  it should "merge with ++=" in {
    val s1 = new State
    val s2 = new State

    val r1 = Var(1, "ex:r1")
    val r2 = Var(2, "ex:r2")

    s1 += r1
    s2 += r2

    // left merge states is done like this
    s1 ++= s2

    s1(r2).value should be (r2.value)
    assert(s1(r2) eq r2)
  }
}

class StateSerialisationTest extends FlatSpec {
  "A state" should "serialise and deserialise in JSON" in {
    var json = """
[
    { "value": 1.0, "identifier": "ex:x1" },
    { "value": false, "identifier": "ex:x2", "scope": "foo" }
]
"""
    val s1 = State.fromJSON(json)
    val s2 = State.fromJSON(State.toJSON(s1))

    assert(s1.table.keySet == s2.table.keySet)
    for (k <- s1.table.keySet) {
      assert(s1(k).key == s2(k).key)
      // RHZ: The problem here seems to be that State.table
      // contains references to `Var[_]`s and we don't know the
      // type of `Var[_].value`.  However, I haven't been able to
      // construct a minimal working example that reproduces this
      // behaviour in the REPL.
      // assert(s1(k).value == s2(k).value)
      // Something like this can be done instead for `BooleanVar`s
      if (s1(k).isInstanceOf[BooleanVar] &&
          s2(k).isInstanceOf[BooleanVar]) {
        println(s1(k) + " == " + s2(k))
        assert(s1(k).value.asInstanceOf[Boolean] ==
               s2(k).value.asInstanceOf[Boolean])
      }
      // But it's of no help for NumericVar because of type erasure
    }
  }
}
*/
