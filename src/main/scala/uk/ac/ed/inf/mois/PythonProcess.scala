package uk.ac.ed.inf.mois

import scala.language.dynamics
import scala.collection.mutable

import org.python.util.PythonInterpreter
import org.python.core.{PyException, PyFloat, PyObject}

abstract class PythonProcess(name: String) extends Process(name) {
  type F = (Double, Double, Seq[DoubleVar]) => Unit
  private val pyFuncs = mutable.ArrayBuffer.empty[(F, Seq[DoubleVar])]

  protected case class py(val vs: DoubleVar*) {
    def := (f: F) = {
      pyFuncs += (f -> vs)
    }
  }

  case class Python(module: String) extends Dynamic {
    private val interp = new PythonInterpreter
    def applyDynamic(func: String)(args: DoubleVar*) = {
      try {
	interp.exec(s"from $module import $func")
      } catch {
	case e: PyException =>
	  throw new IllegalArgumentException(e.toString)
      }

      val fh = interp.get(func)
      if (fh == null)
	throw new IllegalArgumentException(s"no such python function ${module}.${func}")

      def wrapper(t: Double, tau: Double, vs: DoubleVar*) {
	val pyArgs = 
	(Seq(t, tau).map(new PyFloat(_)) ++ args.map(new PyFloat(_)))
	  .toArray
	  .asInstanceOf[Array[PyObject]]
	try {
	  val pyResult = fh.__call__(pyArgs)
	  if (pyResult.isSequenceType) {
	    if (pyResult.__len__ != vs.length)
	      throw new IllegalArgumentException(s"expected ${module}.${func} to return " +
						 s"${vs.length} items and got " +
						 s"${pyResult.__len__}")
	    for (i <- 0 until vs.length) {
	      vs(i) := pyResult.__getitem__(i).asDouble
	    }
	  } else {
	    vs.length match {
	      case 0 =>
	      case 1 => vs(0) := pyResult.asDouble
	      case _ =>
		throw new IllegalArgumentException(s"${module}.${func} didn't return a " +
						   s"sequence and ${vs.length} values required")
	    }
	  }
	} catch {
	  case e: PyException =>
	    throw new IllegalArgumentException(e.toString)
	}
      }
      wrapper _
    }
  }

  def step(t: Double, tau: Double) {
    for ((f, vs) <- pyFuncs) {
      f(t, tau, vs)
    }
  }
}