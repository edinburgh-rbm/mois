package uk.ac.ed.inf.mois

import scala.collection.mutable

/** A `ConstraintViolation` is raised when a restriction on a `Var`
  * is violated.
  */
case class ConstraintViolation(msg: String) extends Exception(msg)

/** This class is to abstract away the details of uniquely identifying a
  * state variable.
  */
class VarMeta(val identifier: String) extends Ordered[VarMeta] {
  def compare(that: VarMeta): Int = this.identifier compare that.identifier
}

/** A `Var` is basically a named value of a certain type. It is operated
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
abstract class Var[T] {

  protected[mois] def value: T
  protected[mois] def value_= (x: T)

  val meta: VarMeta

  /** Syntax sugar for assignment. */
  @inline final def :=(x: T): this.type = this.update(x)

  def stringPrefix = "Var"

  override def toString =
    stringPrefix + "(" + meta + ") := " + value

  // -- Abstract members --

  /** Assignment to a Variable is expected to set the underlying value. */
  def update(x: T): this.type
}

abstract class BooleanVar extends Var[Boolean] {
  override def stringPrefix = "Boolean"
}

abstract class NumericVar[T: Numeric] extends Var[T] {
  override def stringPrefix = value match {
    case _: Int => "Int"
    case _: Long => "Long"
    case _: Float => "Float"
    case _: Double => "Double"
  }

  def +=(that: T) = update (implicitly[Numeric[T]].plus(value, that))
  def -=(that: T) = update (implicitly[Numeric[T]].minus(value, that))
  def *=(that: T) = update (implicitly[Numeric[T]].times(value, that))
  // def /=(that: T) = update (value / that)
  // def %=(that: T) = update (value % that)
}

/** A `Var` Handle or pointer. This is used mostly by `Process` to enable
 * variables to be defined in process scope, and to be referred to by a name
 * in that scope, but at the same time have the underlying variable be mutable
 * so that when it is replaced -- perhaps when adding to a `ProcessGroup` --
 * the reference remains useable.
 */
trait VarRef[T] extends Var[T] {
  //type V <: VarContainer#VarVal[T]
  //var ref: V
  val ref: Var[T]

  protected[mois] def value = ref.value
  protected[mois] def value_= (x: T) = ref.value = x
  
  /** Assignment to a Variable is expected to set the underlying value. */
  def update(x: T): this.type = { ref.update(x); this }
}

abstract class BooleanVarRef(val meta: VarMeta)
	 extends BooleanVar with VarRef[Boolean]
abstract class NumericVarRef[T: Numeric](val meta: VarMeta)
	 extends NumericVar[T] with VarRef[T]

trait VarContainer {

  trait VarVal[T] extends Var[T] {
    // It could be a ListBuffer as well
    val constraints: mutable.ArrayBuffer[Constraint] =
      mutable.ArrayBuffer.empty[Constraint]

    type Constraint = T => Boolean

    /** Assignment to a Variable is expected to set the underlying value. */
    override def update(x: T): this.type = {
      for (c <- constraints if c(x))
        throw new ConstraintViolation("variable " + this +
          " violated a constraint by setting its value to " + x)
      value = x
      this
    }

    object AddConstraint {
      def and(c: Constraint) = must(c)
    }

    /** Adds a constraint to this variable. */
    def must(constraint: Constraint) = {
      constraints += constraint
      AddConstraint
    }
  }

  class BooleanVarVal(val meta: VarMeta)
        extends BooleanVar with VarVal[Boolean] {
    var value: Boolean = false
  }

  // TODO: We probably want to split this class into 4 classes for
  // each Numeric type.
  class NumericVarVal[T: Numeric](val meta: VarMeta)
        extends NumericVar[T] with VarVal[T] {
    var value: T = implicitly[Numeric[T]].zero
  }

  val ints = mutable.ArrayBuffer.empty[NumericVarRef[Int]]
  val longs = mutable.ArrayBuffer.empty[NumericVarRef[Long]]
  val floats = mutable.ArrayBuffer.empty[NumericVarRef[Float]]
  val doubles = mutable.ArrayBuffer.empty[NumericVarRef[Double]]
  val bools = mutable.ArrayBuffer.empty[BooleanVarRef]

  private def numericVarRef[T: Numeric](
    meta: VarMeta, 
    pool: mutable.ArrayBuffer[NumericVarRef[T]]) =
  {
    object varref extends NumericVarRef[T](meta) {
      val ref = new NumericVarVal[T](meta)
    }
    pool += varref
    varref
  }

  private def booleanVarRef(
    meta: VarMeta,
    pool: mutable.ArrayBuffer[BooleanVarRef]) = 
  {
    object varref extends BooleanVarRef(meta) {
      val ref = new BooleanVarVal(meta)
    }
    pool += varref
    varref
  }

  def Int(meta: VarMeta) = numericVarRef(meta, ints)
  def Long(meta: VarMeta) = numericVarRef(meta, longs)
  def Float(meta: VarMeta) = numericVarRef(meta, floats)
  def Double(meta: VarMeta) = numericVarRef(meta, doubles)
  def Boolean(meta: VarMeta) = booleanVarRef(meta, bools)

  implicit def StringMeta(s: String) = new VarMeta(s)
}



