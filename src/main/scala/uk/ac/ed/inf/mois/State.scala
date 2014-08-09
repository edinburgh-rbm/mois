package uk.ac.ed.inf.mois

import scala.collection.mutable
import scala.language.implicitConversions
import scala.language.postfixOps
import spire.algebra.Rig
import spire.math.{Complex, Natural, Rational, Real}
import spire.implicits._

/** State is just a list of variables, one array for each type
  * and a corresponding list of metadata that is used to build
  * an index so the state can be conveniently accessed.
  */
case class State(
  val meta: Map[Rig[_], Array[VarMeta]],
  val vars: Map[Rig[_], Array[_]] // XX would be nice to enforce same type here
)

/** The State companion object provides an implicit function
  * to retrieve an arrayof the correct type just by referencing
  * the state itself. For example,
  *
  * {{{
  * val doubles: Array[Double] = state
  * }}}
  */
object State {
  implicit def getArray[T](s: State)(implicit rig: Rig[T]): Array[T] =
    s.vars(rig).asInstanceOf[Array[T]]
}

/** An Index is used to access a specific [[State]] variable.
  * It is instantiated lazily, and it is important to call
  * [[Index.setState]] before using it, otherwise null pointer
  * errors will result.
  *
  * @param meta is used to find the right array and offset
  *             into the state
  */
class Index[T](val meta: VarMeta)(implicit val rig: Rig[T]) {
  private var _state: State = null
  private lazy val state = _state
  private lazy val array: Array[T] = state
  private lazy val index = state.meta(rig) indexOf meta

  /** setState must be called once and only once before using
    * the index
    */
  def setState(s: State) {
    require(_state == null, "setState may not be called more than once")
    _state = s
  }

  /** Add an [[Annotation]] onto the [[VarMeta]] */
  val annotate = meta.annotate _

  /** Explicitly retrieve the underlying value in the state */
  @inline final def value = array(index)
  /** Update the underlying value in the state */
  @inline final def update(value: T) { array(index) = value }
  /** Syntax sugar for updating the underlying value in the state */
  @inline final def :=(value: T) = update(value)

  @inline def +(other: T) = rig.plus(value, other)
  @inline def +=(other: T) = update(rig.plus(value, other))
  @inline def *(other: T) = rig.times(value, other)
  @inline def *=(other: T) = update(rig.times(value, other))

  override def toString = s"$meta = $value"
}

/** Index companion object provides implicit function to retrieve
  * the underlying value in the state automatically
  */
object Index {
  @inline implicit def value[T](i: Index[T]): T = i.value
}

/** StateBuilder is used, unsurprisingly, to build a [[State]]. It
  * provides facilities for adding variables to its own mutable data
  * structures, merging information from other StateBuilders, and so
  * forth, and when this is all done, creating a compact and useable
  * [[State]].
  *
  * It is very important to call [[StateBuilder.initStateIndices]]
  * once and only once before using any state.
  *
  * Example:
  * {{{
  * class Foo extends StateBuilder {
  *   val x = Double("x")
  *   val y = Boolean("y")
  *   val z = BigInt("z")
  *
  *   val state = buildState
  *   initStateIndices(state)
  *
  *   // now x, y, z can be used as desired
  * }
  * }}}
  */
trait StateBuilder {
  private[mois] val _vmeta = mutable.Map.empty[Rig[_], mutable.ArrayBuffer[VarMeta]]
  private[mois] val indices = mutable.ArrayBuffer.empty[Index[_]]

  /** Construct a [[State]] */
  def buildState = State(
    _vmeta map { case (rig, metas) => (rig, metas.sorted.toArray) } toMap,
    _vmeta map { case (rig, metas) => (rig, Array.fill(metas.size)(rig.zero)) } toMap
  )

  /** Initialise all indices */
  def initStateIndices(s: State) {
    indices map(_.setState(s))
  }

  /** Add a variable to the under construction proto[[State]]
    *
    * @param meta is the metadata used to index the state
    * @return a handle that can be used to access this variable later.
    */
  def addVar[T](meta: VarMeta)(implicit rig: Rig[T]): Index[T] = {
    // we have never seen any variable of this type
    if (!(_vmeta contains rig))
      _vmeta += rig -> new mutable.ArrayBuffer[VarMeta]
    val vmeta = _vmeta(rig)

    // check that we do not already know this variable
    val allmeta = _vmeta.values.toList.foldLeft(mutable.ArrayBuffer.empty[VarMeta])(
      (z, r) => z ++ r
    )
    require(!(allmeta contains meta), s"$meta already added as a variable")

    vmeta += meta

    val i = new Index(meta)
    indices += i
    i
  }

  /** Merge a partially built [[State]] with this one */
  def merge(other: StateBuilder) {
    for ((rig, metas) <- other._vmeta) {
      if (!(_vmeta contains rig)) {
        _vmeta += rig -> metas.clone
      } else {
        for(m <- metas) {
          if (!(_vmeta(rig) contains m)) {
            _vmeta(rig) += m
          }
        }
      }
    }
  }

  object Int {
    def apply(ident: String) = addVar[Int](new VarMeta(ident))
  }
  object Byte {
    def apply(ident: String) = addVar[Byte](new VarMeta(ident))
  }
  object Long {
    def apply(ident: String) = addVar[Long](new VarMeta(ident))
  }
  object Real {
    def apply(ident: String) = addVar[Real](new VarMeta(ident))
  }
  object Float {
    def apply(ident: String) = addVar[Float](new VarMeta(ident))
  }
  object Short {
    def apply(ident: String) = addVar[Short](new VarMeta(ident))
  }
  object BigInt {
    def apply(ident: String) = addVar[BigInt](new VarMeta(ident))
  }
  object Double {
    def apply(ident: String) = addVar[Double](new VarMeta(ident))
  }
  object Boolean {
    def apply(ident: String) = addVar[Boolean](new VarMeta(ident))
  }
  object Complex {
    def apply(ident: String) = addVar[Complex[Double]](new VarMeta(ident))
  }
  object Natural {
    def apply(ident: String) = addVar[Natural](new VarMeta(ident))
  }
  object Rational {
    def apply(ident: String) = addVar[Rational](new VarMeta(ident))
  }
  object BigDecimal {
    def apply(ident: String) = addVar[BigDecimal](new VarMeta(ident))
  }
}
