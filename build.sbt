name := "mois"

version := "1.99.0"

scalaVersion := "2.11.1"

crossScalaVersions := Seq("2.11.1", "2.10.4")

// this is needed to get some dependencies like grizzled-slf4j
seq(bintrayResolverSettings:_*)

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.7"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.7"

libraryDependencies += "org.clapper" %% "grizzled-slf4j" % "1.0.2"

libraryDependencies += "log4j" % "log4j" % "1.2.17"

libraryDependencies += "commons-logging" % "commons-logging" % "1.1.3"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3"
