resolvers += Resolver.url(
  "Rally Plugin Releases",
  url("https://dl.bintray.com/rallyhealth/sbt-plugins")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.rallyhealth.sbt" %% "sbt-git-versioning" % "1.4.0")
addSbtPlugin("org.lyranthe.sbt" % "partial-unification" % "1.1.2")
