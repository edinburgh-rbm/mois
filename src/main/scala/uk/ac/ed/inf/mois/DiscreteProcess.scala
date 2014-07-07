package uk.ac.ed.inf.mois

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.collection.mutable

abstract class DiscreteProcess(name: String) extends Process(name) {
  type Func = () => Double

  /** Configurable time step size. This is a discrete time process but
   * nevertheless might be integrated with a continuous time process.
   * This stepSize is the amount of continuous, real, time that a discrete
   * time step can be set to take. Defaults to 1.0, naturally.
   */
  val stepSize = 1.0

  private val vars = mutable.ArrayBuffer.empty[NumericVar[Double]]
  private val funcs = mutable.ArrayBuffer.empty[Func]

  protected class Next(val v: NumericVar[Double]) {
    def :=(e: Double): Unit = macro DiscreteMacros.createFun
  }

  protected def addNext(v: NumericVar[Double], f: Func) {
    vars += v.copy
    funcs += f
  }

  protected def n(v: NumericVar[Double]) = new Next(v)

  def step(t0: Double, tau: Double) {
    var t = t0
    while (t < t0+tau) {
      for ((v, f) <- vars zip funcs) {
	v := f()
      }
      for (v <- vars) {
	doubleVars(v) := v
      }
      t += stepSize
      for (sh <- stepHandlers)
        sh.handleStep(t, this)
    }
  }

  @inline override def apply(t: Double, tau: Double) = step(t, tau)
}

private object DiscreteMacros {
  def createFun(c: Context)(e: c.Expr[Double]): c.Expr[Unit] = {
    import c.universe._
    // v is the variable for which we are defining the discrete 
    // process. this is just to make the generated code nicer, it
    // could be just val v = q"${c.prefix.tree}.v" as well
    val v = c.prefix.tree match {
      case q"$x.this.n($v)" => v
      case _ => q"${c.prefix.tree}.v"
    }

    // push the rhs into a lambda expression
    val func = q"(() => ${e.tree})"
    c.Expr[Unit](c.resetLocalAttrs(q"addNext($v, $func)"))
  }
}
