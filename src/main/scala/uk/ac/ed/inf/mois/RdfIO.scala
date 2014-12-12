/*
 *  MOIS: RDF Input/Output for annotation
 *  Copyright (C) 2014 University of Edinburgh School of Informatics
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package uk.ac.ed.inf.mois

import com.hp.hpl.jena.rdf.{model => jena}
import com.hp.hpl.jena.vocabulary.{DCTerms, RDF, RDFS}
import spire.algebra.Rig
import spire.implicits._

object MOIS {
  private val m = jena.ModelFactory.createDefaultModel
  val getURI = "https://edinburgh-rbm.github.io/mois/vocabulary/"
  val Model = m.createResource(getURI + "Model")
  val ProcessGroup = m.createResource(getURI + "ProcessGroup")
  val Process = m.createResource(getURI + "Process")
  val Dimension = m.createResource(getURI + "Dimension")
  val Parameter = m.createResource(getURI + "Parameter")
  val Variable = m.createResource(getURI + "Variable")
  val process = m.createProperty(getURI + "process")
  val cclass = m.createProperty(getURI + "class")
  val vvar  = m.createProperty(getURI + "var")
  val slices = m.createProperty(getURI + "slices")
  val units = m.createProperty(getURI + "units")
  val default = m.createProperty(getURI + "default")
  val version = m.createProperty(getURI + "version")
}

object RdfO {
  val rdftype = jena.ModelFactory
    .createDefaultModel
    .createProperty(RDF.getURI + "type")

  def model = {
    val m = jena.ModelFactory.createDefaultModel
    m.setNsPrefix("rdf", RDF.getURI)
    m.setNsPrefix("rdfs", RDFS.getURI)
    m.setNsPrefix("dct", DCTerms.getURI)
    m.setNsPrefix("sim", MOIS.getURI)
    m
  }

  def maybePrefixed(m: jena.Model, s: String) = {
    val sp = s.split(":")
    if (sp.length == 1) {
      s match {
        case "class" => MOIS.cclass.getURI
        case "description" => DCTerms.description.getURI
        case "mois" => MOIS.version.getURI
        case _ => s
      }
    }
    else {
      val pfx = m.getNsPrefixURI(sp(0))
      if (pfx == null) s
      else pfx + sp(1)
    }
  }

  def rdfTypedValue[T](m: jena.Model, v: Var[_], rig: Rig[T]) = {
    val litf = m.asInstanceOf[jena.ModelCon]
    if (rig == Rig[Boolean])
      litf.createTypedLiteral(v.value.asInstanceOf[Boolean])
    else if (rig == Rig[Int])
      litf.createTypedLiteral(v.value.asInstanceOf[Int])
    else if (rig == Rig[Short])
      litf.createTypedLiteral(v.value.asInstanceOf[Short])
    else if (rig == Rig[Float])
      litf.createTypedLiteral(v.value.asInstanceOf[Float])
    else if (rig == Rig[Double])
      litf.createTypedLiteral(v.value.asInstanceOf[Double])
    else
      m.createLiteral(v.value.toString)
  }

  implicit class RdfAnn(a: Annotation) {
    def rdf(m: jena.Model, uri: jena.Resource = null): jena.Model = {
      val litf = m.asInstanceOf[jena.ModelCon]
      val muri = if (uri == null) m.createResource else uri
      
      for ((ns, uri) <- a.prefixes)
        m.setNsPrefix(ns, uri)

      for ((k, v) <- a.annotations)
        m.add(uri,
          m.createProperty(maybePrefixed(m, k)),
          m.createLiteral(v.toString))

      if (a.isInstanceOf[StateBuilder]) {
        val b = a.asInstanceOf[StateBuilder]
        val state = b.getState
        for (r <- state.getTypes)
          for (vm <- state.getMeta(r)) {
            val vuri = m.createResource(maybePrefixed(m, vm.identifier))
            m.add(muri, MOIS.vvar, vuri)
            m.union(vm.rdf(m, vuri))
            val v = state.getVar(vm)(r)
            m.add(vuri, MOIS.default, rdfTypedValue(m, v, r))
          }
      }

      if (a.isInstanceOf[Model]) {
        m.add(muri, rdftype, MOIS.Model)
        val puri = m.createResource
        m.add(muri, MOIS.process, puri)
        val proc = a.asInstanceOf[Model].process
        m.union(proc.rdf(m, puri))
      }

      if (a.isInstanceOf[ProcessGroup]) {
        val pg = a.asInstanceOf[ProcessGroup]
        m.add(muri, rdftype, MOIS.ProcessGroup)
        for (p <- pg.processes) {
          val puri = m.createResource
          m.add(muri, MOIS.process, puri)
          m.union(p.rdf(m, puri))
        }
      } else if (a.isInstanceOf[Process]) {
        m.add(muri, rdftype, MOIS.Process)
      }

      if (a.isInstanceOf[VarMeta]) {
        val v = a.asInstanceOf[VarMeta]
        m.add(muri, rdftype, MOIS.Variable)
        if (v.flags.dimension) {
          m.add(muri, rdftype, MOIS.Dimension)
          m.add(muri, MOIS.slices, litf.createTypedLiteral(v.flags.slices))
        }
        if (v.flags.param) {
          m.add(muri, rdftype, MOIS.Parameter)
        }
      }

      m
    }
  }
}
