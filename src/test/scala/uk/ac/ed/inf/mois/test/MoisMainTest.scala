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
import uk.ac.ed.inf.mois.{MoisMain, Model, Process}
import uk.ac.ed.inf.mois.{TsvWriter, NetCDFWriter,
                          PlotFileWriter, PlotGUIWriter}

class MoisMainTest extends FlatSpec with Matchers {

  val model = new Model {
    val process = new Process("") {
      val x = Double("x")
      val y = Double("y")
      val z = Double("z")
      val i = Int("i")
    }
  }

  def parse(args: String*) = {
    MoisMain.optParser.parse(args, MoisMain.Config())
  }

  def run(args: String*) = {
    MoisMain.main(args.toArray)
    true
  }

  "main class" should "understand TSV step handlers for stdout" in {
    val sh = MoisMain.getStepHandler("tsv:", model)
    sh.isDefined should be (true)
    sh.get.isInstanceOf[TsvWriter] should be (true)
  }

  it should "understand TSV step handlers for files" in {
    val sh = MoisMain.getStepHandler("tsv:/dev/null", model)
    sh.isDefined should be (true)
    sh.get.isInstanceOf[TsvWriter] should be (true)
  }

  it should "not understand mal-formed TSV step handlers" in {
    val sh = MoisMain.getStepHandler("tsv:/dev/null:xxx", model)
    sh.isDefined should be (false)
  }

  it should "understand NetCDF step handlers" in {
    val sh = MoisMain.getStepHandler("netcdf:/dev/null", model)
    sh.isDefined should be (true)
    sh.get.isInstanceOf[NetCDFWriter] should be (true)
  }

  it should "not understand NetCDF step handlers with no filename" in {
    val sh = MoisMain.getStepHandler("netcdf:", model)
    sh.isDefined should be (false)
  }

  it should "not understand mal-formed NetCDF step handlers" in {
    val sh = MoisMain.getStepHandler("netcdf:/dev/null:xxx", model)
    sh.isDefined should be (false)
  }

  it should "understand Plot step handlers" in {
    val sh = MoisMain.getStepHandler("png:/dev/null", model)
    sh.isDefined should be (true)
    sh.get.isInstanceOf[PlotFileWriter] should be (true)
  }

  it should "understand Plot step handlers with arguments" in {
    val sh = MoisMain.getStepHandler("png:/dev/null:x,y", model)
    sh.isDefined should be (true)
    sh.get.isInstanceOf[PlotFileWriter] should be (true)
  }

  it should "not understand mal-formed Plot step handler specs" in {
    val sh = MoisMain.getStepHandler("png:", model)
    sh.isDefined  should be (false)
  }

  it should "understand GUI step handlers" in {
    val sh = MoisMain.getStepHandler("gui", model)
    sh.isDefined should be (true)
    sh.get.isInstanceOf[PlotGUIWriter] should be (true)
  }

  it should "understand GUI step handlers with arguments" in {
    val sh = MoisMain.getStepHandler("gui:x,y,z", model)
    sh.isDefined should be (true)
    sh.get.isInstanceOf[PlotGUIWriter] should be (true)
  }

  it should "parse step handlers from the command line" in {
    val ocfg = parse("model",
                     "-d", "1",
                     "-o", "tsv:",
                     "-o", "netcdf:/dev/null",
                     "TestModel1")
    ocfg should be ('defined)
    val cfg = ocfg.get
    cfg.stepHandlers.size should equal(2)
    val model = cfg.model.get
    val sh1 = cfg.stepHandlers(0)(model)
    val sh2 = cfg.stepHandlers(1)(model)
    sh1.isDefined should be (true)
    sh1.get.isInstanceOf[TsvWriter] should be (true)
    sh2.isDefined should be (true)
    sh2.get.isInstanceOf[NetCDFWriter] should be (true)
  }

  it should "print out information about a model" in {
    run("info", "TestModel1") should be (true)
  }
}
