val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "wobblelab-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions ++= List("-Xcheck-macros", "-Xmax-inlines", "65"),
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.8",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0",
    libraryDependencies += "io.github.arainko" %% "ducktape" % "0.1.0-RC1"
  )
