package uk.ac.ed.inf.mois.test

import org.scalatest.{FlatSpec, Matchers}

import uk.ac.ed.inf.mois.{Var, State, BooleanVar}
import uk.ac.ed.inf.mois.Conversions._

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
    // assert(s(r1) == r1)
    // assert(s(r2) == r2)
    // assert(s(r1.key) == r1)
    // assert(s(r2.key) == r2)
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

    // when we change a variable via the state
    // it should change the variable itself
    s(r1) += 2 + s[Int](r2)
    assert(r1() == 5)

    s(r1) := 0
    assert(r1() == 0)

    // when we change the variable itself
    // it should change the variable via the state
    r1 := 5
    assert(s(r1)().asInstanceOf[Int] == 5)
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
    // assert(a.value == 2)
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

    // assert(s1(r2) == r2)
    s1(r2).value should be (r2.value)
    assert(s1(r2) eq r2)
  }

  it should "diff with -" in {
    val s1 = new State
    val s1r1 = Var(1, "ex:r1")
    val s1r2 = Var(2, "ex:r2")

    s1 += s1r1
    s1 += s1r2

    val s2 = new State
    val s2r1 = Var(3, "ex:r1")
    val s2r3 = Var(4, "ex:r3")

    s2 += s2r1
    s2 += s2r3

    // diff states is done like this
    val ds = s2 - s1

    assert(ds[Int](s1r1)() == 2)
    assert(ds[Int](s1r2)() == -2)
    assert(ds[Int](s2r3)() == 4)
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
