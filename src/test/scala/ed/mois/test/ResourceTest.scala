package ed.mois.test

import ed.mois.{Resource, BoundsViolation}
import ed.mois.ResourceConversions._
import org.scalatest.FlatSpec

class ResourceTest extends FlatSpec {
  "a resource" should "support arithmetic operations" in {
    val ir1 = new Resource(1, "ex:ir1")
    val ir2 = new Resource(2, "ex:ir2")
    assert(ir1() + ir2() == 3) 
  }

  "resources with different identifiers and scopes" should "be different" in {
    val r1 = new Resource(0, "ex:r1")
    var r1a = new Resource(0, "ex:r1", "a")
    var r2 = new Resource(0, "ex:r2")
    var r3 = new Resource(100, "ex:r1") 

    assert((r1 === r1a) == false)
    assert((r1 === r2) == false)
    assert(r1 === r3)
  } 

  "difference on resources" should "yield a new resource" in {
    val r1_0 = new Resource(0.5, "ex:r1")
    val r1_1 = new Resource(0.8, "ex:r1")

    val dr1 = r1_1 - r1_0
    assert(dr1() - 0.3 < 0.000001) // inexact because of floating point
  }

  "constraints on resources" should "be respected" in {
    val r1 = new Resource(0.0, "ex:r1") |>= 0 |<= 2
    var violated = false

    intercept[BoundsViolation] {
      r1 -= 1.0
    }

    intercept[BoundsViolation] {
      r1 += 3.0
    }
  }

  "hokey syntax" should "work..." in {
    val x1 = new Resource(0.0, "ex:x1")
    val x2 = new Resource(1.0, "ex:x2")

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
