package uk.ac.ed.inf.mois

import scala.collection.mutable
import scala.language.existentials
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
  val vars: Map[Rig[_], Array[_]]
) {
  def deepCopy = copy(vars = vars map { case (rig, a) => (rig, a.clone) } toMap)

  def update[T](data: Array[T])(implicit rig: Rig[T]) =
    copy(vars = vars map { case (r, a) =>
      (r, if (r == rig) data else a)
    } toMap)
  @inline final def :=[T](data: Array[T])(implicit rig: Rig[T]) = update(data)

  def getIndex[T](m: VarMeta)(implicit rig: Rig[T]) = {
    if (!(meta contains m.rig) || !(meta(m.rig) contains m))
      throw new NoSuchElementException(s"key not found $m")
    val i = new Index[T](m)
    i.setState(this)
    i
  }
  def getMeta[T](implicit rig: Rig[T]): Array[VarMeta] =
    meta.getOrElse(rig, Array.empty[VarMeta])

  // xxx inefficient!
  def copyTo[T](other: State)(implicit rig: Rig[T]) {
    if (other.meta contains rig) {
      val mine: Array[T] = this
      val theirs: Array[T] = other
      val mymeta = meta(rig)
      val theirmeta = other.meta(rig)
      for (i <- 0 until theirmeta.size)
        theirs(i) = mine(mymeta indexOf theirmeta(i))
    }
  }

  // xxx inefficient!
  def copyFrom[T](other: State)(implicit rig: Rig[T]) {
    if (other.meta contains rig) {
      val mine: Array[T] = this
      val theirs: Array[T] = other
      val mymeta = meta(rig)
      val theirmeta = other.meta(rig)
      for (i <- 0 until theirmeta.size)
        mine(mymeta indexOf theirmeta(i)) = theirs(i)
    }
  }

  def >>> (other: State) {
    copyTo[Byte](other)
    copyTo[Short](other)
    copyTo[Int](other)
    copyTo[Long](other)
    copyTo[BigInt](other)
    copyTo[Float](other)
    copyTo[Double](other)
    copyTo[BigDecimal](other)
    copyTo[Complex[Double]](other)
    copyTo[Real](other)
    copyTo[Rational](other)
    copyTo[Natural](other)
    copyTo[Boolean](other)
  }

  def <<< (other: State) {
    copyFrom[Byte](other)
    copyFrom[Short](other)
    copyFrom[Int](other)
    copyFrom[Long](other)
    copyFrom[BigInt](other)
    copyFrom[Float](other)
    copyFrom[Double](other)
    copyFrom[BigDecimal](other)
    copyFrom[Complex[Double]](other)
    copyFrom[Real](other)
    copyFrom[Rational](other)
    copyFrom[Natural](other)
    copyFrom[Boolean](other)
  }
}

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
    {
   //   println(s"$rig")
   //   println(s"${s.vars(rig)}")
      s.vars(rig).asInstanceOf[Array[T]]
    }
}

/** An Index is used to access a specific [[State]] variable.
  * It is instantiated lazily, and it is important to call
  * [[Index.setState]] before using it, otherwise null pointer
  * errors will result.
  *
  * @param meta is used to find the amht array and offset
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
  @inline final def :=(value: T) = { update(value); this }

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
  def addVar[T](ident: String)(implicit rig: Rig[T]): Index[T] = {
    // we have never seen any variable of this type
    if (!(_vmeta contains rig))
      _vmeta += rig -> new mutable.ArrayBuffer[VarMeta]
    val vmeta = _vmeta(rig)

    val meta = new VarMeta(ident, rig)

    // check that we do not already know this variable
    val allmeta = _vmeta.values.toList.foldLeft(mutable.ArrayBuffer.empty[VarMeta])(
      (z, r) => z ++ r
    )
    require(!(allmeta contains meta), s"$meta already added as a variable")

    vmeta += meta

    val i = new Index[T](meta)
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
    def apply(ident: String) = addVar[Int](ident)
  }
  object Byte {
    def apply(ident: String) = addVar[Byte](ident)
  }
  object Long {
    def apply(ident: String) = addVar[Long](ident)
  }
  object Real {
    def apply(ident: String) = addVar[Real](ident)
  }
  object Float {
    def apply(ident: String) = addVar[Float](ident)
  }
  object Short {
    def apply(ident: String) = addVar[Short](ident)
  }
  object BigInt {
    def apply(ident: String) = addVar[BigInt](ident)
  }
  object Double {
    def apply(ident: String) = addVar[Double](ident)
  }
  object Boolean {
    def apply(ident: String) = addVar[Boolean](ident)
  }
  object Complex {
    def apply(ident: String) = addVar[Complex[Double]](ident)
  }
  object Natural {
    def apply(ident: String) = addVar[Natural](ident)
  }
  object Rational {
    def apply(ident: String) = addVar[Rational](ident)
  }
  object BigDecimal {
    def apply(ident: String) = addVar[BigDecimal](ident)
  }
}
