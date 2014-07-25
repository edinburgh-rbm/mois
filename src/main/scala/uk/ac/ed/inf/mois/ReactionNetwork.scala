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
  type Species <: BaseSpecies
  type Reaction <: BaseReaction

  val species = VarMap.empty[Base, Species]
  override def allVars = super.allVars ++ species

  // -- Species --

  trait BaseSpecies extends NumericVar[Base] {
    this: Species =>

    var value: Base

    type R >: this.type <: Species

    // -- Multiset creation methods --
    def + (p: Multiset) = p + this
    def + (s: Species) = Multiset(this) + s
    def * (m: Int) = Multiset(this -> m)

    // -- Reaction creation methods --
    def -> (p: Multiset) = Reaction(Multiset(this), p)
    def -> (s: Species) = Reaction(Multiset(this), Multiset(s))
    def -> () = Reaction(Multiset(this), Multiset())

    override def stringPrefix = "Species"
  }

  // -- Multisets --

  class Multiset private (val species: Map[Species, Int])
      extends Map[Species, Int] {

    override def toString = (for ((s, n) <- species) yield
      n + "*" + s.meta).mkString(" + ")

    // -- Map methods --
    def get(s: Species) = species get s
    def iterator = species.iterator
    def + [A >: Int](kv: (Species, A)) = {
      val (s, x) = kv
      x match {
        case i: Int => species get s match {
          case Some(j) => new Multiset(species + (s -> (i+j)))
          case None => new Multiset(species + (s -> i))
        }
        case _ => species + kv
      }
    }
    def - (s: Species) = species get s match {
      case Some(i) => if (i == 1) new Multiset(species - s)
                      else new Multiset(species + (s -> (i-1)))
      case None => species
    }
    override def default(s: Species) = 0
    override def foreach[U](f: ((Species, Int)) => U) =
      species foreach f
    override def empty = Multiset()
    override def size = species.size
    def multisize = species.values.sum

    // -- Multiset creation methods --
    def + (kv: (Species, Int)): Multiset = {
      val (s, i) = kv
      species get s match {
        case Some(j) => new Multiset(species + (s -> (i+j)))
        case None => new Multiset(species + kv)
      }
    }
    def + (s: Species): Multiset = this + (s, 1)
    def + (that: Multiset): Multiset =
      if (this.size >= that.size)
        that.foldLeft(this)({ case (m, kv) => m + kv })
      else
        this.foldLeft(that)({ case (m, kv) => m + kv })
    def * (n: Int) = (0 until (n-1)).foldLeft(
      this)({ case (m, _) => m + this })

    // -- Reaction creation methods --
    def -> (m: Multiset) = Reaction(this, m)
    def -> (s: Species) = Reaction(this, Multiset(s))
    def -> () = Reaction(this, Multiset())
  }

  object Multiset {
    def apply(s: (Species, Int), species: (Species, Int)*): Multiset =
      species.foldLeft(empty + s)({ case (m, s) => m + s })
    def apply(species: Species*) = new Multiset(
      species.groupBy(x => x).map({ case (s, ss) => (s, ss.size) }))
    def empty = new Multiset(Map.empty[Species, Int])
  }

  implicit class MultisetMaker(n: Int) {
    def * (s: Species) = Multiset(s -> n)
    def * (p: Multiset) = p * n
    def apply(s: Species) = Multiset(s -> n)
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
    def apply(s: Species) = rhs(s) - lhs(s)
  }

  abstract class BaseReaction extends SimpleReaction {

    // -- Append species to the right-hand side --
    def + (m: Multiset) = Reaction(lhs, rhs + m)
    def + (s: Species) = Reaction(lhs, rhs + s)
    def * (n: Int) = Reaction(lhs, rhs * n)
  }

  implicit class ReactionMaker(u: Unit) {
    def -> (m: Multiset) = Reaction(Multiset.empty, m)
    def -> (s: Species) = Reaction(Multiset.empty, Multiset(s))
  }

  // -- Factories --

  abstract class SpeciesFactory {
    def apply(meta: VarMeta): Species
  }

  val Species: SpeciesFactory

  abstract class ReactionFactory {
    def apply(lhs: Multiset, rhs: Multiset): Reaction
  }

  val Reaction: ReactionFactory
}

