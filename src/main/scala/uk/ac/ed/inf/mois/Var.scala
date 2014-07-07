package uk.ac.ed.inf.mois

import scala.collection.mutable
import scala.reflect.ClassTag

/** A `ConstraintViolation` is raised when a restriction on a `Var`
  * is violated.
  */
case class ConstraintViolation(msg: String) extends Exception(msg)

/** This class is to abstract away the details of uniquely identifying a
  * state variable.
  */
case class VarMeta(identifier: String) extends Ordered[VarMeta] {
  def compare(that: VarMeta): Int = this.identifier compare that.identifier
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

  type Val = T
  type R >: this.type <: Var[T]

  // -- Abstract members --

  def copy: R

  protected[mois] def value: T
  protected[mois] def value_= (x: T)

  // -- Concrete members --

  val meta: VarMeta

  /** Syntax sugar for assignment. */
  @inline final def :=(x: T): this.type = this.update(x)

  def stringPrefix = "Var"

  override def toString =
    stringPrefix + "(" + meta + ") := " + value

  // -- Constraints --

  val constraints: mutable.ArrayBuffer[Constraint] =
    mutable.ArrayBuffer.empty[Constraint]

  type Constraint = T => Boolean

  /** Assignment to a Variable is expected to set the underlying value. */
  def update(x: T): this.type = {
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

class BooleanVar(val meta: VarMeta) extends Var[Boolean] {
  override def stringPrefix = "Boolean"
  var value: Boolean = false
  type R = BooleanVar
  def copy = new BooleanVar(meta) := value
}

class NumericVar[T: Numeric](val meta: VarMeta)
    extends Var[T] {
  override def stringPrefix = value match {
    case _: Int => "Int"
    case _: Long => "Long"
    case _: Float => "Float"
    case _: Double => "Double"
  }
  var value: T = implicitly[Numeric[T]].zero
  type R = NumericVar[T]
  def copy = new NumericVar[T](meta) := value

  def +=(that: T) = update (implicitly[Numeric[T]].plus(value, that))
  def -=(that: T) = update (implicitly[Numeric[T]].minus(value, that))
  def *=(that: T) = update (implicitly[Numeric[T]].times(value, that))
  // def /=(that: T) = update (value / that)
  // def %=(that: T) = update (value % that)
}

class VarMap[T, U <: Var[T]] {
  private val meta = mutable.Map.empty[VarMeta, U]
  def contains(m: VarMeta) = meta contains m
  def apply(m: VarMeta) = meta.apply(m)

  @inline final def size = meta.size

  def update(v: U) = {
    if (meta contains v.meta) {
      meta(v.meta) := v.value
    } else {
      this += v
    }
  }
  @inline final def <<(v: U) = update(v)

  def set(v: U) = {
    meta += v.meta -> v
    v
  }
  @inline final def +=(v: U) = set(v)

  def leftUpdate(vs: VarMap[T, U]) {
    for (v <- vs)
      if (meta contains v.meta)
        meta(v.meta) := v.value
  }
  @inline final def <<<(vs: VarMap[T, U]) = leftUpdate(vs)
  @inline final def >>>(vs: VarMap[T, U]) = vs.leftUpdate(this)

  def map(f: U => Any) = meta map { case (_, v) => f(v) }
  def foreach(f: U => Unit) = meta foreach { case (_, v) => f(v) }
  def toSeq = (for (v <- this) yield v).toSeq
  def copy = {
    val nvm = VarMap.empty[T, U]
    for (v <- this) nvm += v.copy.asInstanceOf[U]
    nvm
  }

  override def toString =
    "(" + toSeq.mkString(", ") + ")"
}

object VarMap {
  def empty[T, U <: Var[T]] = new VarMap[T, U]
}

object VarConv {
  @inline implicit def Var2Meta(v: Var[_]) = v.meta
  @inline implicit def String2Meta(s: String) = new VarMeta(s)
  @inline implicit def getVarValue[T](v: Var[T]) = v.value
}

trait VarContainer {
  @inline implicit def Var2Meta(v: Var[_]) = v.meta
  @inline implicit def String2Meta(s: String) = new VarMeta(s)
  @inline implicit def getVarValue[T](v: Var[T]) = v.value

  val intVars = VarMap.empty[Int, NumericVar[Int]]
  val longVars = VarMap.empty[Long, NumericVar[Long]]
  val floatVars = VarMap.empty[Float, NumericVar[Float]]
  val doubleVars = VarMap.empty[Double, NumericVar[Double]]
  val boolVars = VarMap.empty[Boolean, BooleanVar]
  val allVars = mutable.Map.empty[VarMeta, Var[_]]

  def addVar[T](nv: Var[T]): Var[T] = {
    if (allVars contains nv) {
      updateVar(nv)
    } else {
      val nvc = nv.copy
      allVars += nvc.meta -> nvc
      nvc.value match {
	case i: Int => intVars << nvc.asInstanceOf[NumericVar[Int]]
	case l: Long => longVars << nvc.asInstanceOf[NumericVar[Long]]
	case f: Float => floatVars << nvc.asInstanceOf[NumericVar[Float]]
	case d: Double => doubleVars << nvc.asInstanceOf[NumericVar[Double]]
	case b: Boolean => boolVars << nvc.asInstanceOf[BooleanVar]
      }
      nvc
    }
  }

  def leftMerge(right: VarContainer) {
    for (i <- right.intVars) addVar(i)
    for (l <- right.longVars) addVar(l)
    for (f <- right.floatVars) addVar(f)
    for (d <- right.doubleVars) addVar(d)
    for (b <- right.boolVars) addVar(b)
  }

  def updateVar[T](nv: Var[T]): Var[T] = {
    val v = allVars(nv).asInstanceOf[Var[T]]
    v := nv
    v
  }

  private def leftUpdate(right: VarContainer) {
    intVars <<< right.intVars
    longVars <<< right.longVars
    floatVars <<< right.floatVars
    doubleVars <<< right.doubleVars
    boolVars <<< right.boolVars
  }
  @inline final def <<<(right: VarContainer) = leftUpdate(right)
  @inline final def >>>(right: VarContainer) = right.leftUpdate(this)

  def Int(meta: VarMeta) = {
    val v = new NumericVar[Int](meta)
    addVar(v).asInstanceOf[NumericVar[Int]]
  }
  def Long(meta: VarMeta) = {
    val v = new NumericVar[Long](meta)
    addVar(v).asInstanceOf[NumericVar[Long]]
  }
  def Float(meta: VarMeta) = {
    val v = new NumericVar[Float](meta)
    addVar(v).asInstanceOf[NumericVar[Float]]
  }
  def Double(meta: VarMeta) = {
    val v = new NumericVar[Double](meta)
    addVar(v).asInstanceOf[NumericVar[Double]]
  }
  def Boolean(meta: VarMeta) = {
    val v = new BooleanVar(meta)
    addVar(v).asInstanceOf[BooleanVar]
  }

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
      ("meta" -> ("identifier" -> v.meta.identifier))
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
        val valid = value.isValidLong
        val _ = if (!valid) println("WARNING: integer variable in " +
          "JSON string is not a valid Int or Long, skipping.")
        if valid
        ("meta", meta: JObject) <- fields
        val m = meta.extract[VarMeta]
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
