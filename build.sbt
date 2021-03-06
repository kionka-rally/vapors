name := "vapors-root"
ThisBuild / organization := "com.rallyhealth"

ThisBuild / scalaVersion := Dependencies.Scala_2_11

ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xfatal-warnings",
)

lazy val core = (project in file("core")).settings(
//  addCompilerPlugin(Dependencies.Plugins.kindProjector.cross(CrossVersion.full)),
  libraryDependencies := Seq(
    Dependencies.catsCore,
    Dependencies.catsFree,
    Dependencies.scalaReflect(scalaVersion.value),
  ) ++ Seq(
    // Test-only dependencies
    Dependencies.scalaCheck,
    Dependencies.scalaTest,
    Dependencies.scalaTestPlusScalaCheck,
  ).map(_ % Test)
)

lazy val circe = (project in file("circe"))
  .dependsOn(core)
  .settings(
//    addCompilerPlugin(Dependencies.Plugins.kindProjector.cross(CrossVersion.full)),
    libraryDependencies := Seq(
      Dependencies.circeCore,
    ) ++ Seq(
      // Test-only dependencies
      Dependencies.circeLiteral,
      Dependencies.circeParser,
      Dependencies.scalaCheck,
      Dependencies.scalaTest,
      Dependencies.scalaTestPlusScalaCheck,
    ).map(_ % Test)
  )
