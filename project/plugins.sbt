// this is needed to get some dependencies like grizzled-slf4j
resolvers += Resolver.url("bintray-sbt-plugin-releases",
  url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
    Resolver.ivyStylePatterns)

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.1.1")

resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("com.sksamuel.scoverage" %% "sbt-scoverage" % "0.95.7")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3") 
