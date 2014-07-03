package uk.ac.ed.inf.mois

// RHZ: To remember that we are working we a mutable Map I prefer
// to write mutable.Map
import scala.collection.mutable //.Map

// RHZ: Perhaps State should extend Map?  I know that's painful
/** A `State` is a collection of `Var`. It is implemented as a map or
  * dictionary from the `Var`'s `Key` to the `Var` itself.
  */
class State {

  // RHZ: Maybe it would be better to store `BooleanVar`s and
  // `NumericVar`s in different tables, so we don't have to skip
  // the type-checker with asInstanceOf later.
  val table = mutable.Map.empty[Key, Var[_]]

  /**
   * Syntax sugar: s(v) returns v as it eists in the state. This allows the use of
   * `Var` for querying the state table
   */
  def apply[T](v: Var[_]) = table.apply(v.key).asInstanceOf[Var[T]]

  /** Syntax sugar: `s(k)` returns the `Var` whose `Key` is `k`. */
  def apply[T](k: Key) = table.apply(k).asInstanceOf[Var[T]]

  /** Pass through filter operations to the underlying table. */
  def filter = table.filter _

  /**
   * Syntax sugar: s contains v -- contains predicate for a particular variable
   */
  def contains(v: Var[_]) = table contains v.key
  /**
   * Syntax sugar: s contains k -- contains predicate for a particular key
   */
  def contains(k: Key) = table contains k

  /** The += operator adds a `Var` to the state. */
  def +=(v: Var[_]) = {
    table += v.key -> v
    this
  }

  /**
   * The <<< operator updates this state with the entirety of the other
   * This is a deep copy and does not keep actual references to the
   * other state's variables. If references must be preserved use ++=
   */
  def <<<(other: State) = {
    for ((_,v) <- other.table)
      if (this contains v)
	this(v) := v.value
      else
        this += v.copy
    this
  }

  /**
   * The ++= operator is a shallow copy by reference that can be used
   * to merge states.
   */
  def ++=(other: State) = {
    for ((_, v) <- other.table)
      this += v
    this
  }

  /** The := operator updates the state for the given variable. */
  def :=(v: Var[_]) = {
    if (this contains v)
      this(v) := v.value
    else
      this += v
  }

  /**
   * The - operator means vector subtraction of states. It returns a new
   * state that is the result of subtracting elements pairwise. It is somewhat
   * of an "outer subtraction" in that if there are elements that exist in this
   * state but not in the other those are preserved. Similarly if there are those
   * in the other state but not this one, they are inverted and subtracted.
   */
  def -(s: State) = {
    val ns = new State
    for ((k,v) <- table) {
      if (s.table contains k) {
	ns += v - s(v)
      } else {
	ns += v
      }
    }
    for ((k,v: NumericVar[_]) <- s.table) {
      if (!(table contains k)) {
	ns += -v
      }
    }
    ns
  }

  override def toString = "(" + (for ((_,v) <- table) yield v).mkString(", ") + ")"

  /**
   * The primary purpose of this method is to deep copy a state dictionary
   * so that vector subtraction can be done between states. This is used by
   * `Process.apply` to construct the state difference before and after an
   * iteration of a process has run.
   */
  def copy = {
    val ns = new State
    for ((_, v) <- table) {
      ns += v.copy
    }
    ns
  }
}

object State {

  def apply() = new State

  def fromJSON(s: String) = {
    import org.json4s._
    import org.json4s.native.JsonMethods._
    import Conversions._
    implicit val formats = DefaultFormats

    val state = new State

    val json = parse(s)

    def scope(fields: Seq[JField]): Option[String] = fields find {
      case (name, _) => name == "scope" } collect {
      case (_, JString(scope)) => scope }

    for (jvar <- json.children) {
      val doubles = for {
        JObject(fields) <- jvar
        ("value", JDouble(value)) <- fields
        ("identifier", JString(identifier)) <- fields
      } yield Var(value, identifier, scope(fields))
      val ints = for {
        JObject(fields) <- jvar
        ("value", JInt(value)) <- fields
        if value.isValidInt
        ("identifier", JString(identifier)) <- fields
      } yield Var(value.toInt, identifier, scope(fields))
      val bools = for {
        JObject(fields) <- jvar
        ("value", JBool(value)) <- fields
        ("identifier", JString(identifier)) <- fields
      } yield Var(value, identifier, scope(fields))
      for (v <- doubles ++ ints ++ bools)
        state += v
    }
    state
  }
  // RHZ: toJSON should perhaps be a method in the class (instead of
  // the companion object)
  def toJSON(s: State) = {
    import org.json4s._
    import org.json4s.JsonDSL._
    import org.json4s.native.JsonMethods._

    def jval[T](v: Var[T]) = {
      v.value match {
	case d: Double => JDouble(d)
	case l: Long => JInt(l)
	case i: Int => JInt(i)
	case b: Boolean => JBool(b)
      }
    }
    val json = (for ((_, v) <- s) yield v).map { v =>
      ("value" -> jval(v)) ~
      ("identifier" -> v.identifier) ~
      ("scope" -> v.scope)
    }
    pretty(render(json))

  }
}
