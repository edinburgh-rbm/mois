package uk.ac.ed.inf.mois
import spire.algebra.{Rig, Ring, Field}

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
