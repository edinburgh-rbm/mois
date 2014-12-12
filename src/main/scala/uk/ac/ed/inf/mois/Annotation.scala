/*
 *  MOIS: Annotations
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

import scala.collection.mutable

/**
 * Annotation provides the ability to add arbitrary key-value
 * pairs associated with an object. This is typically used for
 * meta-data such as descriptive names, titles, authorship,
 * units and so forth.
 */
trait Annotation {
  val annotations = mutable.Map.empty[String, Any]
  /** Add an annotation */
  def annotate(k: String, v: Any) = {
    annotations += k -> v
    this
  }

  val prefixes = mutable.Map.empty[String, String]
  def prefix(ns: String, uri: String) = {
    prefixes += ns -> uri
    this
  }
}
