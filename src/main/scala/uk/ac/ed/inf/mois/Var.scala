package uk.ac.ed.inf.mois

import scala.collection.mutable

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

trait VarContainer {

  // RHZ: I'm thinking maybe these should all be Maps and state be a
  // Map[VarMeta, Var[_]] and then vmap is not needed anymore.  In
  // this way we can spare the asInstanceOfs in numeric/booleanVar.
  val ints = mutable.ArrayBuffer.empty[NumericVar[Int]]
  val longs = mutable.ArrayBuffer.empty[NumericVar[Long]]
  val floats = mutable.ArrayBuffer.empty[NumericVar[Float]]
  val doubles = mutable.ArrayBuffer.empty[NumericVar[Double]]
  val bools = mutable.ArrayBuffer.empty[BooleanVar]

  val vmap = mutable.Map.empty[VarMeta, Var[_]]

  def state: Seq[Var[_]] = bools ++ ints ++ longs ++ floats ++ doubles

  private def numericVar[T: Numeric](
    meta: VarMeta,
    pool: mutable.ArrayBuffer[NumericVar[T]]) =
  {
    if (vmap contains meta) vmap(meta).asInstanceOf[NumericVar[T]]
    else {
      val v = new NumericVar[T](meta)
      pool += v
      vmap += (meta -> v)
      v
    }
  }

  private def booleanVar(
    meta: VarMeta,
    pool: mutable.ArrayBuffer[BooleanVar]) = 
  {
    if (vmap contains meta) vmap(meta).asInstanceOf[BooleanVar]
    else {
      val v = new BooleanVar(meta)
      pool += v
      vmap += (meta -> v)
      v
    }
  }

  def Int(meta: VarMeta) = numericVar(meta, ints)
  def Long(meta: VarMeta) = numericVar(meta, longs)
  def Float(meta: VarMeta) = numericVar(meta, floats)
  def Double(meta: VarMeta) = numericVar(meta, doubles)
  def Boolean(meta: VarMeta) = booleanVar(meta, bools)

  implicit def StringMeta(s: String) = new VarMeta(s)

  implicit def getVarValue[T](v: Var[T]) = v.value

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
    val json = for (v <- this.state) yield
      ("value" -> jval(v)) ~ ("meta" -> Serialization.write(v.meta))
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

