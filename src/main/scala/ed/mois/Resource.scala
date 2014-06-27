package ed.mois

case class BoundsViolation(s: String) extends Exception(s) {
}
/*
 * A Resource is basically a named value of a certain type. It is operated
 * on by a Process. The value given in the initialisation can be retrieved
 * and manipulated in the usual way.
 *
 * Some things need to be said about the identifier and the scope. The
 * identifier is meant to be globally unique and could sensibly be an RDF
 * resource, perhaps in shortened curie notation though this is not
 * enforced. This identifier is used to find out if a resource is the
 * same across two or more processes. So an example might be to use
 *
 *     InChi:ZKHQWZAMYRWXGA-KQYNXXCUSA-N
 *
 * for our friend ATP.
 * 
 * Scope is used to create exclusive buckets that contain the same
 * resource. For example a simulation of two cells might have the same
 * substance in both cells but it doesn't make sense to pool it as a
 * resource. Therefore these cells would have different scopes.
 *
 * The effect of this is that when a global state table is derived from
 * the individual states of several processes, their individual state
 * variables or resources are merged only if the identifiers and the
 * scopes match.
 * 
 */ 
class Resource[T](var value: T, val identifier: String, val scope: String = "default") {
  type R = Resource[T]
  def apply(): T = value
  
  def update(newValue: T): T = {
    newValue match {
      case d: Double => {
	if (geq.isDefined) {
	  val bound = geq.get.asInstanceOf[Double]
	  if (d < bound) 
	    throw new BoundsViolation(s"$identifier($newValue < $bound)")
	}
	if (leq.isDefined) {
	  val bound = leq.get.asInstanceOf[Double]
	  if (d > bound)
	    throw new BoundsViolation(s"$identifier - $newValue > $bound")
	}
      }
      case i: Int => {
	if (geq.isDefined) {
	  val bound = geq.get.asInstanceOf[Int]
	  if (i < bound) 
	    throw new BoundsViolation(s"$identifier($newValue < $bound)")
	}
	if (leq.isDefined) {
	  val bound = leq.get.asInstanceOf[Int]
	  if (i > bound)
	    throw new BoundsViolation(s"$identifier - $newValue > $bound")
	}
      }
      case _ => {}
    } 
    value = newValue.asInstanceOf[T]
    value
  }

  /*
   * Syntax sugar for assignment
   */ 
  val := = update _
 
  /*
   * Is it necessary to override the clone method?
   */ 
  override def clone() = {
    val copy = new Resource[T](value, identifier, scope)
    copy.geq = geq
    copy.leq = leq
    copy
  }

  override def toString = identifier + " = " + value.toString

  /*
   * Determines if this resource is the same as another by comparing
   * identifier and scope.
   */ 
  def sameAs(other: Resource[T]): Boolean = {
    identifier == other.identifier && scope == other.scope
  }
  /*
   * Syntax Sugar for sameAs
   */
  val === = sameAs _

  /*
   * Greater or equals than restriction.
   */
  var geq: Option[T] = None
  def |>=(t: T) = {
    geq = Some(t)
    this
  }

  /*
   * Lesser or equals than restriction.
   */
  var leq: Option[T] = None
  def |<=(t: T) = {
    leq = Some(t)
    this
  }

  /*
   * Provide an implementation of - for some common types
   */
  def -[R](that: R): Delta[T] = {
    value match {
      case d: Double => {
	val o = that.asInstanceOf[Resource[Double]].value
	new Delta(d - o, identifier, scope).asInstanceOf[Delta[T]]
      }
      case i: Int => {
	val o = that.asInstanceOf[Resource[Int]].value
	new Delta(i - o, identifier, scope).asInstanceOf[Delta[T]]
      }
      case b: Boolean => {
	val o = that.asInstanceOf[Resource[Boolean]].value
	new Delta(b != o, identifier, scope).asInstanceOf[Delta[T]]
      }
    }
  }

  def +=(that: Double) = update ((value.asInstanceOf[Double] + that).asInstanceOf[T])
  def -=(that: Double) = update ((value.asInstanceOf[Double] - that).asInstanceOf[T])
  def *=(that: Double) = update ((value.asInstanceOf[Double] * that).asInstanceOf[T])
  def /=(that: Double) = update ((value.asInstanceOf[Double] / that).asInstanceOf[T])
  def %=(that: Double) = update ((value.asInstanceOf[Double] % that).asInstanceOf[T])

  def +=(that: Int) = update ((value.asInstanceOf[Int] + that).asInstanceOf[T])
  def -=(that: Int) = update ((value.asInstanceOf[Int] - that).asInstanceOf[T])
  def *=(that: Int) = update ((value.asInstanceOf[Int] * that).asInstanceOf[T])
  def /=(that: Int) = update ((value.asInstanceOf[Int] / that).asInstanceOf[T])
  def %=(that: Int) = update ((value.asInstanceOf[Int] % that).asInstanceOf[T])
}

object ResourceConversions {
  implicit def Resource2Double(r: Resource[Double]) = r.value
  implicit def Resource2Int(r: Resource[Int]) = r.value
  implicit def Resource2Boolean(r: Resource[Boolean]) = r.value
}

// kludgy initialisation
class Delta[T](v: T, i: String, s: String) extends Resource[T](v, i, s) {
  override def toString =  s"Δ($identifier) = $value"
}
