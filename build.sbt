/*
 *  MOIS: SBT Build Instructions
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
name := "mois"

organization := "uk.ac.ed.inf"

version := "1.99.8-SNAPSHOT"

scalaVersion := "2.11.2"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature" )

scalacOptions in Test ++= Seq("-Xcheckinit")

mainClass in Test := Some("uk.ac.ed.inf.mois.MoisMain")

// this is needed to get some dependencies like grizzled-slf4j
seq(bintrayResolverSettings:_*)

resolvers += "ucar-unidata-releases" at
  "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases/"

libraryDependencies += "com.googlecode.matrix-toolkits-java" % "mtj" % "1.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3"

libraryDependencies += "com.github.fommil.netlib" % "all" % "1.1.2" pomOnly()

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.10"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.jfree" % "jfreechart" % "1.0.14"  exclude("xml-apis", "xml-apis")

libraryDependencies += "org.spire-math" %% "spire" % "0.8.2"

libraryDependencies += "edu.ucar" % "netcdf" % "4.3.22"

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

pomExtra := (
  <url>https://gallows.inf.ed.ac.uk/rbm/mois</url>
  <licenses>
    <license>
      <name>GPLv3 or Later</name>
      <url>https://www.gnu.org/licenses/gpl-3.0.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:edinburgh-rbm/mois.git</url>
    <connection>scm:git:git@github.com:edinburgh-rbm/mois.git</connection>
  </scm>
  <developers>
    <developer>
      <id>dominik</id>
      <name>Dominik Bucher</name>
    </developer>
    <developer>
      <id>rhz</id>
      <name>Ricardo Honorato Z</name>
    </developer>
    <developer>
      <id>ww</id>
      <name>William Waites</name>
    </developer>
  </developers>
)

useGpg := true

usePgpKeyHex("84225CBC")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
