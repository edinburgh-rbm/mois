package uk.ac.ed.inf.mois.test

import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.{Var, ConstraintViolation}
import uk.ac.ed.inf.mois.Conversions._

class VarTest extends FlatSpec with Matchers {

  "state variables" should "support arithmetic operations" in {
    val ir1 = Var(1, "ex:ir1")
    val ir2 = Var(2, "ex:ir2")
    // assert(ir1() + ir2() == 3)
    (ir1 + ir2) should be (3)
  }

  it should "not care too much about types for arithmetic" in {
    val r1 = Var(0.0, "ex:float")
    r1 += 1
  }

  it should "distinguish different identifiers and scopes" in {
    val r1 = Var(0, "ex:r1")
    var r1a = Var(0, "ex:r1", Some("a"))
    var r2 = Var(0, "ex:r2")
    var r3 = Var(100, "ex:r1") 

    r1.key should not be (r1a.key)
    r1.key should not be (r2.key)
    r1.key should be (r3.key)
    // assert(!(r1 sameType r1a))
    // assert(!(r1 sameType r2))
    // assert(r1 sameType r3)
  } 

  val precision = 1e-8
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  it should "support difference" in {
    val r1_0 = Var(0.5, "ex:r1")
    val r1_1 = Var(0.8, "ex:r1")

    val dr1 = r1_1 - r1_0
    // assert(dr1() - 0.3 < 0.000001) // inexact because of floating point
    dr1.value should equal (0.3)
  }

  it should "support subtraction with doubles" in {
    val v = Var(0.0, "ex:v")
    // assert(v - 1.0 < -1.0)
    (v - 1.0) should equal (-1.0)
  }

  it should "respect constraints" in {
    val r1 = Var(0.0, "ex:r1")
    r1 must (_ >= 0) and (_ <= 2)

    intercept[ConstraintViolation] {
      r1 -= 1.0
    }

    intercept[ConstraintViolation] {
      r1 += 3.0
    }
  }

  it should "have unambiguous keys" in {
    val v1 = Var(0, "a")
    val v2 = Var(0, "a")
    // assert(v1.key == v2.key)
    v1.key should be (v2.key)

    val v3 = Var(0, "b")
    // assert(v1.key != v3.key)
    v1.key should not be (v3.key)

    val v4 = Var(0, "a", Some("b"))
    // assert(v1.key != v4.key)
    // assert(v3.key != v4.key)
    v1.key should not be (v4.key)
    v3.key should not be (v4.key)

    val v5 = Var(1, "a")
    // assert(v1.key == v5.key)
    v1.key should be (v5.key)
  }

  it should "have natural syntax for numerical operations" in {
    val x1 = Var(0.0, "ex:x1")
    val x2 = Var(1.0, "ex:x2")

    x1 := 2
    // assert(x1() == 2)
    x1.value should be (2)

    x2 := 2 + 2*x1
    // assert(x2() == 6)
    x2.value should be (6)

    val dx = x2 - x1
    // assert(2 * dx == 8)
    (2 * dx) should be (8)

    x1 += dx
    // assert(x1() == x2())
    x1.value should be (x2.value)

    // 1 + dx
    // assert(x1 - dx != x2 - dx)
    // assert( (x1 - dx)() == (x2 - dx)() )
    (x1 - dx) should not be (x2 - dx)
    (x1 - dx).value should be ((x2 - dx).value)
  }
}
