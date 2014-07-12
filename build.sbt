name := "mois"

organization := "uk.ac.ed.inf"

version := "1.99.2-SNAPSHOT"

scalaVersion := "2.11.1"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature" )

// this is needed to get some dependencies like grizzled-slf4j
seq(bintrayResolverSettings:_*)

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.10"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.python" % "jython" % "2.7-b1"

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
