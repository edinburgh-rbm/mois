package uk.ac.ed.inf.mois

/**
 * A `ConstraintViolation` is raised when a restriction on a `Var` is violated
 */
case class ConstraintViolation(s: String) extends Exception(s)

/**
 * This class is to abstract away the details of uniquely identifying a
 * state variable.
 */
class Key(s: String, i: String) extends Tuple2[String, String](s, i) with Ordered[Key] {
  // this is pretty ugly
  def compare(b: Key): Int = {
    if (this._1 == b._1 && this._2 == b._2) 0
    else if (this._1 == b._1) this._2 compare b._2
    else this._1 compare b._1
  }
}

/**
 * A `Var` is basically a named value of a certain type. It is operated
 * on by a Process. The value given in the initialisation can be retrieved
 * and manipulated in the usual way.
 *
 * Some things need to be said about the identifier and the scope. The
 * identifier is meant to be globally unique and could sensibly be an RDF
 * resource, perhaps in shortened curie notation though this is not
 * enforced. This identifier is used to find out if a variable is the
 * same across two or more processes. So an example might be to use
 *
 *     InChi:ZKHQWZAMYRWXGA-KQYNXXCUSA-N
 *
 * for our friend ATP.
 * 
 * Scope is used to create exclusive buckets that contain the same
 * variable. For example a simulation of two cells might have the same
 * substance in both cells but it doesn't make sense to pool it as a
 * variable. Therefore these cells would have different scopes.
 *
 * The effect of this is that when a global state table is derived from
 * the individual states of several processes, their individual state
 * variables or variables are merged only if the identifiers and the
 * scopes match.
 * 
 */ 
object Var {
  def apply(value: Boolean, identifier: String, scope: Option[String]) =
    new BooleanVar(value, identifier, scope)
  def apply(value: Boolean, identifier: String) =
    new BooleanVar(value, identifier, None)
  def apply(value: Float, identifier: String, scope: Option[String]) =
    new NumericVar[Float](value, identifier, scope)
  def apply(value: Float, identifier: String) =
    new NumericVar[Float](value, identifier, None)
  def apply(value: Double, identifier: String, scope: Option[String]) =
    new NumericVar[Double](value, identifier, scope)
  def apply(value: Double, identifier: String) =
    new NumericVar[Double](value, identifier, None)
  def apply(value: Int, identifier: String, scope: Option[String]) =
    new NumericVar[Int](value, identifier, scope)
  def apply(value: Int, identifier: String) =
    new NumericVar[Int](value, identifier, None)
  def apply(value: Long, identifier: String, scope: Option[String]) =
    new NumericVar[Long](value, identifier, scope)
  def apply(value: Long, identifier: String) =
    new NumericVar[Long](value, identifier, None)
}

// RHZ: This is what I wanted to call VarProxy
/** A `Var` Handle or pointer. This is used mostly by `Process` to enable
  * variables to be defined in process scope, and to be referred to by a name
  * in that scope, but at the same time have the underlying variable be mutable
  * so that when it is replaced -- perhaps when adding to a `ProcessGroup` --
  * the reference remains useable.
  */
abstract class VarH[T] {
  def apply(): Var[T]
}

// RHZ: I'd like Var to be the supertype of variables in the state and
// variables "proxies", ie variables in the process state that have to
// ask to the parent process what the value of the variable is each time.
/** The class implementing variables (or values with metadata). This could
  * use some attention, there is far too much introspecting of types going
  * on and there is probably a lot that isn't idiomatic Scala.
  */
abstract class Var[T] {

  var value: T
  val identifier: String
  val scope: Option[String]

  // It could be a ListBuffer as well
  val constraints: collection.mutable.ArrayBuffer[Constraint] =
    collection.mutable.ArrayBuffer.empty[Constraint]

  type Constraint = T => Boolean

  /** Return a Key that will identify this variable by its metadata regardless
    * of its actual value. This is intended to be used as an indexinto dictionaries.
    */
  def key = new Key(if (scope.isDefined) scope.get else "default", identifier)

  /** When a Variable is applied or called, what is expected is its value. */
  def apply(): T = value

  /** Assignment to a Variable is expected to set the underlying value. */
  def update(x: T): this.type = {
    for (c <- constraints if c(value))
      throw new ConstraintViolation(s"variable $identifier violated a " +
        s"constraint by setting its value to $x")
    value = x
    this
  }

  /** Syntax sugar for assignment. */
  @inline final def :=(x: T): this.type = this.update(x)

  /** The `copy` method is primarily to support deep copy of a state
    * dictionary. The deep copy of a state dictionary is primarily to
    * support doing vector subtraction on states (e.g. diffs). It is not
    * recommended to make use of this for other purposes otherwise
    * unexpected things may or may not happen.
    */
  def copy: Var[T]

  override def toString = identifier + " = " + value.toString

  /** Determines if this variable is the same as another by comparing
    * metadata.
    */
  def sameType(other: Var[T]): Boolean = key == other.key
  @inline final def ===(other: Var[T]) = sameType(other)

  object AddConstraint {
    def and(c: Constraint) = should(c)
  }

  /** Add a constraint to this variable. */
  def should(constraint: Constraint) = {
    constraints += constraint
    //AddConstraint
    this
  }

  def -(that: Var[T]): Delta[T]

  def -=(that: T): Var[T]
  def +=(that: T): Var[T]
  def *=(that: T): Var[T]
}

class BooleanVar(
  var value: Boolean,
  val identifier: String,
  val scope: Option[String])
    extends Var[Boolean] {

  def -(that: Var[Boolean]): Delta[Boolean] =
    new Delta[Boolean](value != that.value, identifier, scope)

  def copy = new BooleanVar(value, identifier, scope)

  def -=(that: Boolean): Var[Boolean] = throw new Exception(
    "I don't know yet how to do that to Booleans")
  def +=(that: Boolean): Var[Boolean] = throw new Exception(
    "I don't know yet how to do that to Booleans")
  def *=(that: Boolean): Var[Boolean] = throw new Exception(
    "I don't know yet how to do that to Booleans")
}

class NumericVar[T: Numeric](
  var value: T,
  val identifier: String,
  val scope: Option[String])
    extends Var[T] {

  def copy = new NumericVar[T](value, identifier, scope)

  /** Provide an implementation of - for some common types. */
  def -(that: Var[T]): Delta[T] =
    new Delta[T](implicitly[Numeric[T]].minus(value, that.value),
      identifier, scope)

  def +=(that: T) = update (implicitly[Numeric[T]].plus(value, that))
  def -=(that: T) = update (implicitly[Numeric[T]].minus(value, that))
  def *=(that: T) = update (implicitly[Numeric[T]].times(value, that))
  // def /=(that: T) = update (value / that)
  // def %=(that: T) = update (value % that)

  def unary_- = new NumericVar[T](
    implicitly[Numeric[T]].negate(value), identifier, scope)
}

/*
 * A Delta represents the difference between two variables. The main purpose
 * is to put a fancy unicode triangle before the name. It might be useful for
 * other purposes as well.
 */
// kludgy initialisation
class Delta[T](v: T, i: String, s: Option[String]) extends Var[T] { //(v, i, s) {

  // FIXME
  var value = v
  val identifier = i
  val scope = s

  override def toString =  s"Δ($identifier) = $value"
  def copy = new Delta[T](v, i, s)
  def -(that: Var[T]): Delta[T] = throw new Exception(
    "I don't know yet how to substract values to Deltas")
  def -=(that: T): Delta[T] = throw new Exception(
    "I don't know yet how to do that to Deltas")
  def +=(that: T): Delta[T] = throw new Exception(
    "I don't know yet how to do that to Deltas")
  def *=(that: T): Delta[T] = throw new Exception(
    "I don't know yet how to do that to Deltas")
}

/*
 * Methods for converting between Var and fundamental types
 */
object Conversions {
  implicit def Var2Value[T](v: Var[T]) = v.value
  implicit def Delta2Value[T](v: Delta[T]) = v.value

  implicit def VarH2Double(v: VarH[Double]) = v().value
  implicit def VarH2Float(v: VarH[Float]) = v().value
  implicit def VarH2Int(v: VarH[Int]) = v().value
  implicit def VarH2Long(v: VarH[Long]) = v().value
  implicit def VarH2Boolean(v: VarH[Boolean]) = v().value

  implicit def VarH2Var[T](v: VarH[T]) = v()
}
