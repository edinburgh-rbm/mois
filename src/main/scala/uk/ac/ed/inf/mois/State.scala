package uk.ac.ed.inf.mois

import scala.collection.mutable
import scala.language.existentials
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.reflect.ClassTag

import spire.algebra.Rig
import spire.math.{Complex, Natural, Rational, Real}
import spire.implicits._

/** State is just a list of variables, one array for each type
  * and a corresponding list of metadata that is used to build
  * an index so the state can be conveniently accessed.
  */
case class State(
  val meta: mutable.Map[Rig[_], Array[VarMeta]],
  val vars: mutable.Map[Rig[_], Array[_]]) {

  def deepCopy = copy(
    vars = mutable.Map.empty[Rig[_],Array[_]] ++ vars.map({
      case (rig, a) => (rig, a.clone) }))

  // RHZ: After using update, indices don't work anymore (they point
  // to the old state).
  // def update[T](data: Array[T])(implicit rig: Rig[T]) =
  //   copy(vars = vars + (rig -> data))
  def update[T](data: Array[T])(implicit rig: Rig[T]) =
    vars += (rig -> data)

  @inline final def := [T](data: Array[T])(implicit rig: Rig[T]) =
    update(data)

  def getIndex[T](m: VarMeta)(implicit rig: Rig[T]) = {
    if (!(meta contains m.rig) || !(meta(m.rig) contains m))
      throw new NoSuchElementException(s"key not found $m")
    val i = new Index[T](m)
    i.setState(this)
    i
  }

  def getMeta[T](implicit rig: Rig[T]): Array[VarMeta] =
    meta.getOrElse(rig, Array.empty[VarMeta])

  def get[T](implicit rig: Rig[T]): Array[T] =
    vars(rig).asInstanceOf[Array[T]]

  // xxx inefficient!
  def copyTo[T](other: State)(implicit rig: Rig[T]) {
    if (other.meta contains rig) {
      val mine: Array[T] = this.get[T]
      val theirs: Array[T] = other.get[T]
      val mymeta = meta(rig)
      val theirmeta = other.meta(rig)
      for (i <- 0 until theirmeta.size)
        theirs(i) = mine(mymeta indexOf theirmeta(i))
    }
  }

  // xxx inefficient!
  def copyFrom[T](other: State)(implicit rig: Rig[T]) {
    if (other.meta contains rig) {
      val mine: Array[T] = this.get[T]
      val theirs: Array[T] = other.get[T]
      val mymeta = meta(rig)
      val theirmeta = other.meta(rig)
      for (i <- 0 until theirmeta.size)
        mine(mymeta indexOf theirmeta(i)) = theirs(i)
    }
  }

  // FIXME: This should be just a for over meta and vars instead
  def >>> (other: State) {
    for (rig <- other.meta.keys if meta contains rig)
      copyTo(other)(rig)
  }

  def <<< (other: State) {
    for (rig <- meta.keys if other.meta contains rig)
      copyFrom(other)(rig)
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
  // RHZ: I really think it's not good to do this kind of implicit
  // conversion because what you get back is not the state, it's
  // something else and that should be apparent, as in state.get[T]
  // implicit def getArray[T](s: State)(implicit rig: Rig[T])
  //     : Array[T] = {
  //   // println(s"$rig")
  //   // println(s"${s.vars(rig)}")
  //   s.vars(rig).asInstanceOf[Array[T]]
  // }
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
  // RHZ: Why do we need to cache state?
  lazy val state = _state
  def array: Array[T] = state.get[T]
  private lazy val index = state.meta(rig) indexOf meta

  // RHZ: Why not just expose state_=?
  /** setState must be called once and only once before using
    * the index
    */
  def setState(s: State) {
    require(_state == null, "setState may not be called more than once")
    _state = s
  }

  // RHZ: Why having a Function1 instead of a simple method?
  /** Add an [[Annotation]] onto the [[VarMeta]] */
  val annotate = meta.annotate _

  /** Explicitly retrieve the underlying value in the state */
  @inline final def value = array(index)
  /** Update the underlying value in the state */
  @inline final def update(value: T) { array(index) = value }
  /** Syntax sugar for updating the underlying value in the state */
  @inline final def := (value: T) = { update(value); this }

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

  class Bag[T: ClassTag](implicit rig: Rig[T]) {
    self =>
    val metas = mutable.ArrayBuffer.empty[VarMeta]
    def add(meta: VarMeta) = metas += meta
    def values: Array[T] = Array.fill[T](metas.size)(rig.zero)
    def copy: Bag[T] = new Bag[T] {
      override val metas = self.metas.clone
    }
  }

  private[mois] val bags = mutable.Map.empty[Rig[_], Bag[_]]
  private[mois] val indices = mutable.ArrayBuffer.empty[Index[_]]
  private[mois] val allmeta = mutable.Set.empty[VarMeta]

  // RHZ: Why sorted? Why not just keep the order in which the user
  // declared the variables?
  /** Construct a [[State]] */
  def buildState = State(
    mutable.Map.empty[Rig[_],Array[VarMeta]] ++ bags.map({
      case (rig, bag) => (rig, bag.metas.toArray) }),
    mutable.Map.empty[Rig[_],Array[_]] ++ bags.map({
      case (rig, bag) => (rig, bag.values)
    })
  )

  /** Initialise all indices */
  def initStateIndices(s: State) {
    indices map (_.setState(s))
  }

  /** Add a variable to the under construction proto[[State]]
    *
    * @param meta is the metadata used to index the state
    * @return a handle that can be used to access this variable later.
    */
  def addVar[T: ClassTag](ident: String)(implicit rig: Rig[T]): Index[T] = {
    val meta = new VarMeta(ident, rig)
    // check that we do not already know this variable
    if (!(allmeta contains meta)) {
      allmeta += meta
      // we have never seen any variable of this type
      if (!(bags contains rig))
        bags(rig) = new Bag[T]
      bags(rig) add meta
    } else {
      // we already have a variable called this, and are just being
      // asked for another index for it. make sure index is the right
      // type
      require(bags(rig).metas contains meta, "requested index for wrong type")
    }
    val i = new Index[T](meta)
    indices += i
    i
  }

  /** Merge a partially built [[State]] with this one */
  def merge(other: StateBuilder) {
    for ((rig, bag) <- other.bags) {
      if (!(bags contains rig)) {
        bags(rig) = bag.copy.asInstanceOf[Bag[_]]
      } else {
        for(m <- bag.metas) {
          if (!(bags(rig).metas contains m)) {
            bags(rig) add m
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
