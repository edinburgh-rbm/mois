package uk.ac.ed.inf.mois

import scala.language.implicitConversions

/** A base trait for reaction networks.  These reaction network use
  * species as variables and let you define reactions using them.
  * This is the base class for DeterministicReactionNetwork,
  * StochasticReactionNetwork and FBA.
  */
trait ReactionNetwork extends BaseProcess {

  override def stringPrefix = "ReactionNetwork"

  // All species in a ReactionNetwork must be copy numbers
  // (Int or Long) or concentrations (Float or Double).
  // This doesn't allow for hybrid models.
  type Base // for counting number of ocurrences of species
  type Specie <: BaseSpecie
  type Reaction <: BaseReaction

  val species = VarMap.empty[Base, Specie]
  override def allVars = super.allVars ++ species

  // private val rxns = mutable.ArrayBuffer.empty[Reaction]
  // def reactions(rs: Reaction*) = for (r <- rs) {
  //   rxns += r
  //   for (s <- r.lhs) species += s
  //   for (s <- r.rhs) species += s
  // }

  // -- Species --

  trait BaseSpecie extends NumericVar[Base] {
    this: Specie =>

    var value: Base

    type R >: this.type <: Specie

    // -- Multiset creation methods --
    def + (p: Multiset) = p + this
    def + (s: Specie) = Multiset(this) + s
    def * (m: Int) = Multiset(this -> m)

    // -- Reaction creation methods --
    def -> (p: Multiset) = Reaction(Multiset(this), p)
    def -> (s: Specie) = Reaction(Multiset(this), Multiset(s))
    def -> () = Reaction(Multiset(this), Multiset())

    override def stringPrefix = "Specie"
  }

  // -- Multisets --

  class Multiset private (val species: Map[Specie, Int])
      extends Map[Specie, Int] {

    override def toString = (for ((s, n) <- species) yield
      n + "*" + s.meta).mkString(" + ")

    // -- Map methods --
    def get(s: Specie) = species get s
    def iterator = species.iterator
    def + [A >: Int](kv: (Specie, A)) = {
      val (s, x) = kv
      x match {
        case i: Int => species get s match {
          case Some(j) => new Multiset(species + (s -> (i+j)))
          case None => new Multiset(species + (s -> i))
        }
        case _ => species + kv
      }
    }
    def - (s: Specie) = species get s match {
      case Some(i) => if (i == 1) new Multiset(species - s)
                      else new Multiset(species + (s -> (i-1)))
      case None => species
    }
    override def default(s: Specie) = 0
    override def foreach[U](f: ((Specie, Int)) => U) =
      species foreach f
    override def empty = Multiset()
    override def size = species.size
    def multisize = species.values.sum

    // -- Multiset creation methods --
    def + (kv: (Specie, Int)): Multiset = {
      val (s, i) = kv
      species get s match {
        case Some(j) => new Multiset(species + (s -> (i+j)))
        case None => new Multiset(species + kv)
      }
    }
    def + (s: Specie): Multiset = this + (s, 1)
    def + (that: Multiset): Multiset =
      if (this.size >= that.size)
        that.foldLeft(this)({ case (m, kv) => m + kv })
      else
        this.foldLeft(that)({ case (m, kv) => m + kv })
    def * (n: Int) = (0 until (n-1)).foldLeft(
      this)({ case (m, _) => m + this })

    // -- Reaction creation methods --
    def -> (m: Multiset) = Reaction(this, m)
    def -> (s: Specie) = Reaction(this, Multiset(s))
    def -> () = Reaction(this, Multiset())
  }

  object Multiset {
    def apply(s: (Specie, Int), species: (Specie, Int)*): Multiset =
      species.foldLeft(empty + s)({ case (m, s) => m + s })
    def apply(species: Specie*) = new Multiset(
      species.groupBy(x => x).map({ case (s, ss) => (s, ss.size) }))
    def empty = new Multiset(Map.empty[Specie, Int])
  }

  implicit class MultisetMaker(n: Int) {
    def * (s: Specie) = Multiset(s -> n)
    def * (p: Multiset) = p * n
    def apply(s: Specie) = Multiset(s -> n)
    def apply(p: Multiset) = p * n
  }

  // -- Reactions --

  abstract class SimpleReaction {

    val lhs: Multiset
    val rhs: Multiset

    override def toString = "Reaction(" + lhs + ", " + rhs + ")"
    override def equals(that: Any) = that match {
      case that: BaseReaction =>
        (this.lhs == that.lhs) && (this.rhs == that.rhs)
      case _ => false
    }
    def apply(s: Specie) = rhs(s) - lhs(s)
  }

  abstract class BaseReaction extends SimpleReaction {

    // -- Append species to the right-hand side --
    def + (m: Multiset) = Reaction(lhs, rhs + m)
    def + (s: Specie) = Reaction(lhs, rhs + s)
    def * (n: Int) = Reaction(lhs, rhs * n)
  }

  implicit class ReactionMaker(u: Unit) {
    def -> (m: Multiset) = Reaction(Multiset.empty, m)
    def -> (s: Specie) = Reaction(Multiset.empty, Multiset(s))
  }

  // -- Factories --

  abstract class SpecieFactory {
    def apply(meta: VarMeta): Specie
  }

  val Specie: SpecieFactory

  abstract class ReactionFactory {
    def apply(lhs: Multiset, rhs: Multiset): Reaction
  }

  val Reaction: ReactionFactory
}

