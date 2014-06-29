package uk.ac.ed.inf.mois.test

import org.scalatest.FlatSpec

import uk.ac.ed.inf.mois.{Var, State}
import uk.ac.ed.inf.mois.Conversions._

class StateTest extends FlatSpec {
  "a state" should "support adding and retrieving variables" in {
    val s = new State
    val r1 = Var(1, "ex:r1")
    val r2 = Var(2, "ex:r2")

    // adding a resource to state is done like this
    s += r1
    s += r2

    assert(s(r1) == r1)
    assert(s(r2) == r2)
    assert(s(r1.key) == r1)
    assert(s(r2.key) == r2)
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

  "two states" should "merge with ++=" in {
    val s1 = new State
    val s2 = new State

    val r1 = Var(1, "ex:r1")
    val r2 = Var(2, "ex:r2")

    s1 += r1
    s2 += r2

    // merge states is done like this
    s1 ++= s2

    assert(s1(r2) == r2)
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

    println(s1)
    println(s2)
    println(ds)
    assert(ds[Int](s1r1)() == 2)
    assert(ds[Int](s1r2)() == -2)
    assert(ds[Int](s2r3)() == 4)
  }
}
