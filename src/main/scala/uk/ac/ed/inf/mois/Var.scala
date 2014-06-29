package uk.ac.ed.inf.mois

/*
 * A `BoundsViolation` is raised when a restriction on a `Var` is violated
 */
case class BoundsViolation(s: String) extends Exception(s) {
}

/*
 * This class is to abstract away the details of uniquely identifying a
 * state variable.
 */
class Key(s: String, i: String) extends Tuple2[String, String](s, i) {}

/*
 * A `Var` is basically a named value of a certain type. It is operated
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
object Var {
  def apply[T](value: T, identifier: String, scope: String = "default") =
    new Var(value, identifier, scope)
}

abstract class VarH[T] {
  def apply(): Var[T]
}


class Var[T](var value: T, val identifier: String, val scope: String) {
  type R = Var[T]

  /*
   * Return a Key that will identify this variable by its metadata regardless
   * of its actual value. This is intended to be used as an indexinto dictionaries.
   */
  def key() = new Key(scope, identifier)

  /*
   * When a Variable is applied or called, what is expected is its value
   */
  def apply(): T = value
  
  /*
   * Assignment to a Variable is expected to set the underlying value
   */
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
      case i: Long => {
	if (geq.isDefined) {
	  val bound = geq.get.asInstanceOf[Long]
	  if (i < bound) 
	    throw new BoundsViolation(s"$identifier($newValue < $bound)")
	}
	if (leq.isDefined) {
	  val bound = leq.get.asInstanceOf[Long]
	  if (i > bound)
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
   * The `copy` method is primarily to support deep copy of a state
   * dictionary. The deep copy of a state dictionary is primarily to
   * support doing vector subtraction on states (e.g. diffs). It is not
   * recommended to make use of this for other purposes otherwise
   * unexpected things may or may not happen.
   */
  def copy = {
    val nv = new Var[T](value, identifier, scope)
    nv.geq = geq
    nv.leq = leq
    nv
  }

  override def toString = identifier + " = " + value.toString

  /*
   * Determines if this resource is the same as another by comparing
   * metadata
   */ 
  def sameAs(other: Var[T]): Boolean = {
    key == other.key
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
	val o = that.asInstanceOf[Var[Double]].value
	new Delta(d - o, identifier, scope).asInstanceOf[Delta[T]]
      }
      case i: Long => {
	val o = that.asInstanceOf[Var[Long]].value
	new Delta(i - o, identifier, scope).asInstanceOf[Delta[T]]
      }
      case i: Int => {
	val o = that.asInstanceOf[Var[Int]].value
	new Delta(i - o, identifier, scope).asInstanceOf[Delta[T]]
      }
      case b: Boolean => {
	val o = that.asInstanceOf[Var[Boolean]].value
	new Delta(b != o, identifier, scope).asInstanceOf[Delta[T]]
      }
    }
  }

  def +=(that: Double) = update ((value.asInstanceOf[Double] + that).asInstanceOf[T])
  def -=(that: Double) = update ((value.asInstanceOf[Double] - that).asInstanceOf[T])
  def *=(that: Double) = update ((value.asInstanceOf[Double] * that).asInstanceOf[T])
  def /=(that: Double) = update ((value.asInstanceOf[Double] / that).asInstanceOf[T])
  def %=(that: Double) = update ((value.asInstanceOf[Double] % that).asInstanceOf[T])

  def +=(that: Long) = update ((value.asInstanceOf[Long] + that).asInstanceOf[T])
  def -=(that: Long) = update ((value.asInstanceOf[Long] - that).asInstanceOf[T])
  def *=(that: Long) = update ((value.asInstanceOf[Long] * that).asInstanceOf[T])
  def /=(that: Long) = update ((value.asInstanceOf[Long] / that).asInstanceOf[T])
  def %=(that: Long) = update ((value.asInstanceOf[Long] % that).asInstanceOf[T])

  def +=(that: Int) = update ((value.asInstanceOf[Int] + that).asInstanceOf[T])
  def -=(that: Int) = update ((value.asInstanceOf[Int] - that).asInstanceOf[T])
  def *=(that: Int) = update ((value.asInstanceOf[Int] * that).asInstanceOf[T])
  def /=(that: Int) = update ((value.asInstanceOf[Int] / that).asInstanceOf[T])
  def %=(that: Int) = update ((value.asInstanceOf[Int] % that).asInstanceOf[T])
}

/*
 * Methods for converting between Var and fundamental types
 */
object Conversions {
  implicit def Var2X[T](v: Var[T]) = v.value

  implicit def VarH2Double(v: VarH[Double]) = v().value
  implicit def VarH2Long(v: VarH[Long]) = v().value
  implicit def VarH2Int(v: VarH[Int]) = v().value
  implicit def VarH2Boolean(v: VarH[Boolean]) = v().value

  implicit def VarH2Var[T](v: VarH[T]) = v()
}

// kludgy initialisation
class Delta[T](v: T, i: String, s: String) extends Var[T](v, i, s) {
  override def toString =  s"Δ($identifier) = $value"
}

