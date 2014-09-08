package uk.ac.ed.inf.mois
import spire.algebra.{Rig, Ring, Field}

class RigSyntax[T : Rig](v: Var[T]) {
  def += (y: T) { v.update(Rig[T].plus(v.value, y)) }
  def *= (y: T) { v.update(Rig[T].times(v.value, y)) }
}

class RingSyntax[T: Ring](v: Var[T]) {
  def -= (y: T) { v.update(Ring[T].plus(v.value, Ring[T].negate(y))) }
}

class FieldSyntax[T: Field](v: Var[T])
