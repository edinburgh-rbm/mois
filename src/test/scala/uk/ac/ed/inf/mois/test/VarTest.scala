package uk.ac.ed.inf.mois.test

import org.scalatest.FlatSpec

import uk.ac.ed.inf.mois.{Var, BoundsViolation}
import uk.ac.ed.inf.mois.Conversions._

class VarTest extends FlatSpec {
  "state variables" should "support arithmetic operations" in {
    val ir1 = Var(1, "ex:ir1")
    val ir2 = Var(2, "ex:ir2")
    assert(ir1() + ir2() == 3) 
  }

  it should "distinguish different identifiers and scopes" in {
    val r1 = Var(0, "ex:r1")
    var r1a = Var(0, "ex:r1", "a")
    var r2 = Var(0, "ex:r2")
    var r3 = Var(100, "ex:r1") 

    assert((r1 === r1a) == false)
    assert((r1 === r2) == false)
    assert(r1 === r3)
  } 

  it should "support difference" in {
    val r1_0 = Var(0.5, "ex:r1")
    val r1_1 = Var(0.8, "ex:r1")

    val dr1 = r1_1 - r1_0
    assert(dr1() - 0.3 < 0.000001) // inexact because of floating point
  }

  ignore should "support subtraction with doubles"in {
    val v = Var(0.0, "ex:v")
    assert(v - 1.0 < -1.0)
  }

  it should "respect constraints" in {
    val r1 = Var(0.0, "ex:r1") |>= 0 |<= 2
    var violated = false

    intercept[BoundsViolation] {
      r1 -= 1.0
    }

    intercept[BoundsViolation] {
      r1 += 3.0
    }
  }

  it should "have unambiguous keys" in {
    val v1 = Var(0, "a")
    val v2 = Var(0, "a")
    assert(v1.key == v2.key)

    val v3 = Var(0, "b")
    assert(v1.key != v3.key)

    val v4 = Var(0, "a", "b")
    assert(v1.key != v4.key)
    assert(v3.key != v4.key)

    val v5 = Var(1, "a")
    assert(v1.key == v5.key)
  }

  it should "natural syntax for numerical operaitons" in {
    val x1 = Var(0.0, "ex:x1")
    val x2 = Var(1.0, "ex:x2")

    x1 := 2
    assert(x1() == 2)

    x2 := 2 + 3*x1
    assert(x2() == 8)

    x2() = 2 + 2*x1 
    assert(x2() == 6)

    val dx = x2 - x1
    assert(2 * dx == 8)

    x1 += dx
    assert(x1() == x2())

    1 + dx
    assert(x1 - dx != x2 - dx)
    assert( (x1 - dx)() == (x2 - dx)() )
  }
}
