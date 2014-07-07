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

class IntVar(val meta: VarMeta) extends Var[Int] {
  override def stringPrefix = "Int"
  var value = 0
  type R = IntVar
  def copy = new IntVar(meta) := value
  def +=(that: Int) = update (value + that)
  def -=(that: Int) = update (value - that)
  def *=(that: Int) = update (value * that)
  def /=(that: Int) = update (value / that)
  def %=(that: Int) = update (value % that)
}

class LongVar(val meta: VarMeta) extends Var[Long] {
  override def stringPrefix = "Long"
  var value = 0L
  type R = LongVar
  def copy = new LongVar(meta) := value
  def +=(that: Long) = update (value + that)
  def -=(that: Long) = update (value - that)
  def *=(that: Long) = update (value * that)
  def /=(that: Long) = update (value / that)
  def %=(that: Long) = update (value % that)
}

class FloatVar(val meta: VarMeta) extends Var[Float] {
  override def stringPrefix = "Float"
  var value = (0.0).toFloat
  type R = FloatVar
  def copy = new FloatVar(meta) := value
  def +=(that: Float) = update (value + that)
  def -=(that: Float) = update (value - that)
  def *=(that: Float) = update (value * that)
  def /=(that: Float) = update (value / that)
  def %=(that: Float) = update (value % that)
}

class DoubleVar(val meta: VarMeta) extends Var[Double] {
  override def stringPrefix = "Double"
  var value = 0.0
  type R = DoubleVar
  def copy = new DoubleVar(meta) := value
  def +=(that: Double) = update (value + that)
  def -=(that: Double) = update (value - that)
  def *=(that: Double) = update (value * that)
  def /=(that: Double) = update (value / that)
  def %=(that: Double) = update (value % that)
}

class VarMap[T, U <: Var[T] { type R = U }] extends mutable.Map[VarMeta, U] {

  private val meta = mutable.Map.empty[VarMeta, U]

  // -- mutable.Map methods --
  override def get(key: VarMeta): Option[U] = meta.get(key)
  override def iterator: Iterator[(VarMeta, U)] = meta.iterator
  override def += (kv: (VarMeta, U)) = { meta += kv; this }
  override def -= (key: VarMeta) = { meta -= key; this }

  def add(v: U): U = get(v.meta) match {
    case Some(myv) => myv := v.value
    case None => {
      val copy = v.copy
      this += v.meta -> copy
      copy
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
  def empty[T, U <: Var[T] { type R = U }] = new VarMap[T, U]
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

  val intVars = VarMap.empty[Int, IntVar]
  val longVars = VarMap.empty[Long, LongVar]
  val floatVars = VarMap.empty[Float, FloatVar]
  val doubleVars = VarMap.empty[Double, DoubleVar]
  val boolVars = VarMap.empty[Boolean, BooleanVar]

  def allVars: mutable.Map[VarMeta, Var[_]] =
    intVars ++ longVars ++ floatVars ++ doubleVars ++ boolVars

  def addVar(nv: IntVar) = intVars add nv
  def addVar(nv: LongVar) = longVars add nv
  def addVar(nv: FloatVar) = floatVars add nv
  def addVar(nv: DoubleVar) = doubleVars add nv
  def addVar(nv: BooleanVar) = boolVars add nv

  def leftMerge(right: VarContainer) {
    for (i <- right.intVars.values) addVar(i)
    for (l <- right.longVars.values) addVar(l)
    for (f <- right.floatVars.values) addVar(f)
    for (d <- right.doubleVars.values) addVar(d)
    for (b <- right.boolVars.values) addVar(b)
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

  def Int(meta: VarMeta) = addVar(new IntVar(meta))
  def Long(meta: VarMeta) = addVar(new LongVar(meta))
  def Float(meta: VarMeta) = addVar(new FloatVar(meta))
  def Double(meta: VarMeta) = addVar(new DoubleVar(meta))
  def Boolean(meta: VarMeta) = addVar(new BooleanVar(meta))

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
    // RHZ: Why not just Serialization.write(v.meta) instead?  That
    // way we don't have to change this code when VarMeta changes.
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
