package uk.ac.ed.inf.mois
import spire.algebra.{Order, Rig, Ring, Field}

final class RigVarSyntax[T : Rig](v: Var[T]) {
  def += (y: T) { v.update(Rig[T].plus(v.value, y)) }
  def *= (y: T) { v.update(Rig[T].times(v.value, y)) }
}

final class RingVarSyntax[T: Ring](v: Var[T]) {
  def -= (y: T) { v.update(Ring[T].plus(v.value, Ring[T].negate(y))) }
}

final class FieldVarSyntax[T: Field](v: Var[T]) {
  def /= (y: T) { v.update(Field[T].div(v.value, y)) }
  def %= (y: T) { v.update(Field[T].mod(v.value, y)) }
}

final class StateSyntax(s: State) {
  @inline final def := [T](data: Array[T])(implicit rig: Rig[T]) =
    s.update(data)
  @inline final def >>> (other: State) = s.copyToAll(other)
  @inline final def <<< (other: State) = s.copyFromAll(other)
}

final class VarSyntax[T](v: Var[T]) {
  def param() = {
    v.meta.flags.param = true
    v
  }
  def dimension() = {
    v.meta.flags.dimension = true
    v
  }
  def dimension(n: Int) = {
    v.meta.flags.dimension = true
    v.meta.flags.slices = n
    v
  }
}

// XXX this belongs *inside* the following class!
class Dimension[T](v: Var[T]) {
  def +=(n: Int) {
    v.meta.flags.slices += n
  }
}

final class DimensionSyntax(s: StateBuilder) {
  def dimension[T](v: Var[T]) = new Dimension(v)
}

final class ConstraintSyntax[T](v: Var[T]) {
  object addConstraint {
    def and(c: v.Constraint) = {
      v.addConstraint(c)
      this
    }
  }
  def must(c: v.Constraint) = {
    v.addConstraint(c)
    addConstraint
  }
  def assertConstraints = v.doAssertConstraints(v.value)
  def checkConstraints = v.doCheckConstraints(v.value)
}

final class BoundSyntax[T : Order : Rig](v: Var[T]) {
  def gte(b: T) = { v.lowerBound = Some(new v.LowerBound(b)); v }
  def lte(b: T) = { v.upperBound = Some(new v.UpperBound(b)); v }
  def nonnegative()(implicit r: Rig[T], o: Order[T]) = gte(r.zero)
}
