package ed.mois.test

import ed.mois.{Resource, State}
import ed.mois.ResourceConversions._
import org.scalatest.FlatSpec

class StateTest extends FlatSpec {
  "a state" should "support adding and retrieving resources" in {
    val s = new State
    val r1 = new Resource(1, "ex:r1")
    val r2 = new Resource(2, "ex:r2")

    // adding a resource to state is done like this
    s += r1
    s += r2

    assert(s(r1) == r1)
    assert(s(r2) == r2)
    assert(s((r1.identifier, r1.scope)) == r1)
    assert(s((r2.identifier, r2.scope)) == r2)

    intercept[NoSuchElementException] {
      s((r1.identifier, r2.identifier))
    }
  }

  "arithmetic on a state variable" should "work as with resources directly" in {
    var s = new State
    val r1 = new Resource(1, "ex:r1")
    val r2 = new Resource(2, "ex:r2")

    s += r1
    s += r2

    s(r1) += 2 + s[Int](r2)

    assert(r1() == 5)

    s(r1) := 0
    assert(r1() == 0)
  }

  "two states" should "merge with ++=" in {
    val s1 = new State
    val s2 = new State

    val r1 = new Resource(1, "ex:r1")
    val r2 = new Resource(2, "ex:r2")

    s1 += r1
    s2 += r2

    // merge states is done like this
    s1 ++= s2

    assert(s1(r2) == r2)
  }

  it should "diff with -" in {
    val s1 = new State
    val s1r1 = new Resource(1, "ex:r1")
    val s1r2 = new Resource(2, "ex:r2")

    s1 += s1r1
    s1 += s1r2

    val s2 = new State
    val s2r1 = new Resource(3, "ex:r1")
    val s2r3 = new Resource(4, "ex:r3")

    s2 += s2r1
    s2 += s2r3

    // diff states is done like this
    val ds = s2 - s1

    assert(ds[Int](s1r1)() == 2)
    assert(ds[Int](s1r2)() == 0)
    assert(ds[Int](s2r3)() == 4)
  }
}
