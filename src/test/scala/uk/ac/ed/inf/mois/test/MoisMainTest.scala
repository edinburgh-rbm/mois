/*
 *  MOIS: MoisMain Test
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
package uk.ac.ed.inf.mois.test

import org.scalatest.{FlatSpec, Matchers}
import uk.ac.ed.inf.mois.MoisMain
import uk.ac.ed.inf.mois.{TsvWriter, NetCDFWriter, PlotWriter}

class MoisMainTest extends FlatSpec with Matchers {
  def parse(args: String*) = {
    MoisMain.optParser.parse(args, MoisMain.Config())
  }

  def run(args: String*) = {
    MoisMain.main(args.toArray)
    true
  }
  
  "main class" should "understand TSV step handlers for stdout" in {
    val sh = MoisMain.getStepHandler("tsv:")
    sh.isDefined should be (true)
    sh.get.isInstanceOf[TsvWriter] should be (true)
  }

  it should "understand TSV step handlers for files" in {
    val sh = MoisMain.getStepHandler("tsv:/dev/null")
    sh.isDefined should be (true)
    sh.get.isInstanceOf[TsvWriter] should be (true)
  }

  it should "not understand mal-formed TSV step handlers" in {
    val sh = MoisMain.getStepHandler("tsv:/dev/null:xxx")
    sh.isDefined should be (false)
  }

  it should "understand NetCDF step handlers" in {
    val sh = MoisMain.getStepHandler("netcdf:/dev/null")
    sh.isDefined should be (true)
    sh.get.isInstanceOf[NetCDFWriter] should be (true)
  }

  it should "not understand NetCDF step handlers with no filename" in {
    val sh = MoisMain.getStepHandler("netcdf:")
    sh.isDefined should be (false)
  }

  it should "not understand mal-formed NetCDF step handlers" in {
    val sh = MoisMain.getStepHandler("netcdf:/dev/null:xxx")
    sh.isDefined should be (false)
  }

  it should "understand Plot step handlers" in {
    val sh = MoisMain.getStepHandler("plot:/dev/null")
    sh.isDefined should be (true)
    sh.get.isInstanceOf[PlotWriter] should be (true)
  }

  it should "understand Plot step handlers with arguments" in {
    val sh = MoisMain.getStepHandler("plot:/dev/null:x,y")
    sh.isDefined should be (true)
    sh.get.isInstanceOf[PlotWriter] should be (true)
  }

  it should "not understand mal-formed Plot step handler specs" in {
    val sh = MoisMain.getStepHandler("plot:")
    sh.isDefined  should be (false)
  }

  it should "parse step handlers from the command line" in {
    val ocfg = parse("model",
		    "-d", "1",
		    "-o", "tsv:",
		    "-o", "netcdf:/dev/null",
		    "TestModel1")
    ocfg should not equal (None)
    val cfg = ocfg.get
    cfg.stepHandlers.size should equal(2)
    cfg.stepHandlers(0).isDefined should be (true)
    cfg.stepHandlers(0).get.isInstanceOf[TsvWriter] should be (true)
    cfg.stepHandlers(1).isDefined should be (true)
    cfg.stepHandlers(1).get.isInstanceOf[NetCDFWriter] should be (true)
  }

  it should "print out information about a model" in {
    run("info", "TestModel1") should be (true)
  }
}
