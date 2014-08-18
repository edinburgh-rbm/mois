/*
 *  MOIS: Variable Types
 *  Copyright (C) 2014 University of Edinburgh School of Informatics
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package uk.ac.ed.inf.mois

import scala.collection.mutable
import scala.language.implicitConversions

/** A `ConstraintViolation` is raised when a restriction on a `Var`
  * is violated.
  */
case class ConstraintViolation(msg: String) extends Exception(msg)

/** This class is to abstract away the details of uniquely identifying a
  * state variable.
  */
case class VarMeta(identifier: String)
    extends Ordered[VarMeta] with Annotation {
  def compare(that: VarMeta) = this.identifier compare that.identifier
  override def toString = identifier
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

  type R >: this.type <: Var[T]

  // -- Abstract members --

  def copy: R

  protected[mois] def value: T
  protected[mois] def value_= (x: T)

  // -- Constraints --

  // RHZ: I wanted to move constraints to VarMeta but VarMeta doesn't
  // know about T.
  type Constraint = T => Boolean
  val constraints: mutable.ArrayBuffer[Constraint] =
    mutable.ArrayBuffer.empty[Constraint]

  // Could we merge modifiers (bounds, like NonZero) with constraints?
  abstract class Modifier {
    def apply(x: T): T
  }
  val modifiers : mutable.ArrayBuffer[Modifier] =
    mutable.ArrayBuffer.empty[Modifier]

  // def merge(that: VarMeta): VarMeta = {
  //   require(this.identifier == that.identifier,
  //     "impossible to merge VarMetas with different identifiers")
  //   for (c <- that.constraints)
  //     constraints += c
  // }

  // -- Concrete members --

  val meta: VarMeta

  /** Syntax sugar for assignment. */
  @inline final def := (x: T): this.type = this.update(x)

  def stringPrefix = "Var"

  override def toString =
    stringPrefix + "(" + meta + ") := " + value

  /** Assignment to a Variable is expected to set the underlying value. */
  def update(x: T): this.type = {
    for (c <- constraints if !c(x))
      throw new ConstraintViolation("variable " + this +
        " violated a constraint by setting its value to " + x)
    value = modifiers.foldLeft(x)((x, m) => m(x))
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

  /** Adds an annotation to variable metadata */
  def annotate(k: String, v: String) = {
    meta.annotate(k, v)
  }
}

class BooleanVar(val meta: VarMeta) extends Var[Boolean] {
  override def stringPrefix = "Boolean"
  var value: Boolean = false
  type R = BooleanVar
  def copy = new BooleanVar(meta) := value
}

trait NumericVar[T] extends Var[T] {
  type R >: this.type <: NumericVar[T]
  def += (x: T): this.type
  def -= (x: T): this.type
  def *= (x: T): this.type
  def /= (x: T): this.type
  def %= (x: T): this.type

  class LowerBound(b: T)(implicit num: Numeric[T]) extends Modifier {
    def apply(x: T) = if (num.lteq(x, b)) b else x
  }
  class UpperBound(b: T)(implicit num: Numeric[T]) extends Modifier {
    def apply(x: T) = if (num.gteq(x, b)) b else x
  }

  object clip {
    def gte(b: T)(implicit num: Numeric[T]) = {
      modifiers += new LowerBound(b)
      this
    }
    def lte(b: T)(implicit num: Numeric[T]) = {
      modifiers += new UpperBound(b)
      this
    }
    def nonnegative()(implicit num: Numeric[T]) = {
      modifiers += new LowerBound(num.zero)
      this
    }
  }
  def gte(b: T)(implicit num: Numeric[T]) = clip.gte(b)
  def lte(b: T)(implicit num: Numeric[T]) = clip.lte(b)
  def nonnegative()(implicit num: Numeric[T]) = clip.gte(num.zero)
}

trait IntVarIntf extends NumericVar[Int] {
  type R >: this.type <: IntVarIntf
  override def stringPrefix = "Int"
  var value = 0
  def +=(that: Int) = update (value + that)
  def -=(that: Int) = update (value - that)
  def *=(that: Int) = update (value * that)
  def /=(that: Int) = update (value / that)
  def %=(that: Int) = update (value % that)
}

class IntVar(val meta: VarMeta) extends IntVarIntf {
  type R = IntVar
  def copy = new IntVar(meta) := value
}

trait LongVarIntf extends NumericVar[Long] {
  type R >: this.type <: LongVarIntf
  override def stringPrefix = "Long"
  var value = 0L
  def +=(that: Long) = update (value + that)
  def -=(that: Long) = update (value - that)
  def *=(that: Long) = update (value * that)
  def /=(that: Long) = update (value / that)
  def %=(that: Long) = update (value % that)
}

class LongVar(val meta: VarMeta) extends LongVarIntf {
  type R = LongVar
  def copy = new LongVar(meta) := value
}

trait FloatVarIntf extends NumericVar[Float] {
  type R >: this.type <: FloatVarIntf
  override def stringPrefix = "Float"
  var value = (0.0).toFloat
  def +=(that: Float) = update (value + that)
  def -=(that: Float) = update (value - that)
  def *=(that: Float) = update (value * that)
  def /=(that: Float) = update (value / that)
  def %=(that: Float) = update (value % that)
}

class FloatVar(val meta: VarMeta) extends FloatVarIntf {
  type R = FloatVar
  def copy = new FloatVar(meta) := value
}

trait DoubleVarIntf extends NumericVar[Double] {
  type R >: this.type <: DoubleVarIntf
  override def stringPrefix = "Double"
  var value = 0.0
  def +=(that: Double) = update (value + that)
  def -=(that: Double) = update (value - that)
  def *=(that: Double) = update (value * that)
  def /=(that: Double) = update (value / that)
  def %=(that: Double) = update (value % that)
}

class DoubleVar(val meta: VarMeta) extends DoubleVarIntf {
  type R = DoubleVar
  def copy = new DoubleVar(meta) := value
}

class ArrayVar[T](val meta: VarMeta)
   extends Var[mutable.ArrayBuffer[T]] {
  override def stringPrefix = "Array"
  val value: mutable.ArrayBuffer[T] = mutable.ArrayBuffer.empty[T]
  def value_= (that: Iterable[T]) {
    value.clear
    for (x <- that) value += x
  }
  def value_= (that: mutable.ArrayBuffer[T]) {
    value.clear
    for (x <- that) value += x
  }
  type R = ArrayVar[T]
  def copy = {
    val a = new ArrayVar[T](meta)
    for (x <- value) a.value += x
    a
  }
}

class VarMap[T, U <: Var[T] { type R <: U }] extends mutable.Map[VarMeta, U] {

  private val meta = mutable.Map.empty[VarMeta, U]

  // -- mutable.Map methods --
  override def get(key: VarMeta): Option[U] = meta.get(key)
  override def iterator: Iterator[(VarMeta, U)] = meta.iterator
  override def += (kv: (VarMeta, U)) = { meta += kv; this }
  override def -= (key: VarMeta) = { meta -= key; this }

  def add(v: U): U = get(v.meta) match {
    case Some(myv) => myv := v.value
    case None => {
      this += v.meta -> v
      v
    }
  }

  def leftUpdate(vm: collection.Map[VarMeta, U]) {
    for ((m, v) <- vm)
      if (meta contains m)
        meta(m) := v.value
  }
  @inline final def <<<(vm: collection.Map[VarMeta, U]) = leftUpdate(vm)
  @inline final def >>>(vm: VarMap[T, U]) = vm.leftUpdate(this)

  def copy = {
    val nvm = VarMap.empty[T, U]
    for ((m, v) <- this) nvm(m) = v.copy
    nvm
  }

  override def empty = VarMap.empty[T, U]

  override def toString =
    "(" + toSeq.mkString(", ") + ")"
}

object VarMap {
  def empty[T, U <: Var[T] { type R <: U }] = new VarMap[T, U]
}

trait VarConversions {
  @inline implicit def Var2Meta(v: Var[_]) = v.meta
  @inline implicit def getVarValue[T](v: Var[T]) = v.value
}

trait VarMapConversions {

  /** Implicit utility class for extra operations on a group of DoubleVars */
  implicit class NumericVarMap[T, U <: NumericVar[T] { type R <: U }](
    vm: VarMap[T, U])(implicit num: Numeric[T]) {

    /** Returns an instance with the same variable identifiers,
      * but all values set to zero.
      */
    def zeros = {
      val nvm = vm.copy
      for ((_, v) <- nvm) v := num.zero
      nvm
    }
    def + (other: VarMap[T, U]) = {
      val nvm = vm.copy
      nvm += other
    }
    def + (x: T) = {
      val nvm = vm.copy
      for (v <- nvm.values) v += x
      nvm
    }
    def - (other: VarMap[T, U]) = {
      val nvm = vm.copy
      nvm -= other
    }
    def - (x: T) = {
      val nvm = vm.copy
      for (v <- nvm.values) v -= x
      nvm
    }
    def * (other: VarMap[T, U]) = {
      val nvm = vm.copy
      nvm *= other
    }
    def * (x: T) = {
      val nvm = vm.copy
      for (v <- nvm.values) v *= x
      nvm
    }
    def / (other: VarMap[T, U]) = {
      val nvm = vm.copy
      nvm /= other
    }
    def / (x: T) = {
      val nvm = vm.copy
      for (v <- nvm.values) v /= x
      nvm
    }
    def += (other: VarMap[T, U]) = {
      for ((m, v) <- vm if other contains m)
        v += other(m).value
      vm
    }
    def -= (other: VarMap[T, U]) = {
      for ((m, v) <- vm if other contains m)
        v -= other(m).value
      vm
    }
    def *= (other: VarMap[T, U]) = {
      for ((m, v) <- vm if other contains m)
        v *= other(m).value
      vm
    }
    def /= (other: VarMap[T, U]) = {
      for ((m, v) <- vm if other contains m)
        v /= other(m).value
      vm
    }
  }
}

trait VarContainer extends VarMapConversions {

  @inline implicit def String2Meta(s: String) = new VarMeta(s)

  val intVars = VarMap.empty[Int, IntVar]
  val longVars = VarMap.empty[Long, LongVar]
  val floatVars = VarMap.empty[Float, FloatVar]
  val doubleVars = VarMap.empty[Double, DoubleVar]
  val boolVars = VarMap.empty[Boolean, BooleanVar]

  def allVars: mutable.Map[VarMeta, Var[_]] =
    intVars ++ longVars ++ floatVars ++ doubleVars ++ boolVars

  def leftMerge(right: VarContainer) {
    for (i <- right.intVars.values) intVars add i.copy
    for (l <- right.longVars.values) longVars add l.copy
    for (f <- right.floatVars.values) floatVars add f.copy
    for (d <- right.doubleVars.values) doubleVars add d.copy
    for (b <- right.boolVars.values) boolVars add b.copy
  }

  def leftUpdate(right: VarContainer) {
    intVars <<< right.intVars
    longVars <<< right.longVars
    floatVars <<< right.floatVars
    doubleVars <<< right.doubleVars
    boolVars <<< right.boolVars
  }
  @inline final def <<<(right: VarContainer) = leftUpdate(right)
  @inline final def >>>(right: VarContainer) = right.leftUpdate(this)

  def Int(meta: VarMeta) = intVars add (new IntVar(meta))
  def Long(meta: VarMeta) = longVars add (new LongVar(meta))
  def Float(meta: VarMeta) = floatVars add (new FloatVar(meta))
  def Double(meta: VarMeta) = doubleVars add (new DoubleVar(meta))
  def Boolean(meta: VarMeta) = boolVars add (new BooleanVar(meta))

  // FIXME: There's no annotation for constraints in the JSON format

  /** Creates a JSON string with all variables in this VarContainer. */
  def toJSON: String = {
    import org.json4s._
    import org.json4s.JsonDSL._
    import org.json4s.native.JsonMethods.{pretty, render}
    import org.json4s.native.Serialization

    implicit val formats = Serialization.formats(NoTypeHints)

    def jval[T](v: Var[T]): JValue = {
      v.value match {
        case i: Int => JInt(i)
        case l: Long => JInt(l)
        case f: Float => JDouble(f)
        case d: Double => JDouble(d)
        case b: Boolean => JBool(b)
      }
    }
    val json = for ((_, v) <- this.allVars) yield
      ("value" -> jval(v)) ~
      ("meta" -> Serialization.write(v.meta))
    pretty(render(json))
  }

  /** Parses a JSON string and adds all variable definitions that it
    * finds to this VarContainer.
    *
    * @return the `Seq` of added variables.
    */
  def fromJSON(s: String): Seq[Var[_]] = {
    import org.json4s._
    import org.json4s.native.JsonMethods.parse

    implicit val formats = DefaultFormats
    val json = parse(s)

    for (jvar <- json.children) yield {
      val doubles = for {
        JObject(fields) <- jvar
        ("value", JDouble(value)) <- fields
        ("meta", meta: JObject) <- fields
      } yield Double(meta.extract[VarMeta]) := value
      val ints = for {
        JObject(fields) <- jvar
        ("value", JInt(value)) <- fields
        valid = value.isValidLong
        _ = if (!valid) println("WARNING: integer variable in " +
          "JSON string is not a valid Int or Long, skipping.")
        if valid
          ("meta", meta: JObject) <- fields
        m = meta.extract[VarMeta]
      } yield if (value.isValidInt) Int(m) := value.toInt
              else Long(m) := value.toLong
      val bools = for {
        JObject(fields) <- jvar
        ("value", JBool(value)) <- fields
        ("meta", meta: JObject) <- fields
      } yield Boolean(meta.extract[VarMeta]) := value
      List.empty[Var[_]] ++ doubles ++ ints ++ bools
    }
  }.flatten
}
