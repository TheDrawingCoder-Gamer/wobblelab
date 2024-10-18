val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "wobblelab-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions ++= List("-Xcheck-macros", "-Xmax-inlines", "65"),
    libraryDependencies += "org.typelevel" %% "cats-parse" % "1.0.0",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0"
  )
