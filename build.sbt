import java.nio.file.Files
import cats.syntax.all._
import cats.effect.{IO => CIO}
import org.http4s.ember.server._
import com.comcast.ip4s._
import org.http4s.server
import server.staticcontent._
import org.http4s.HttpRoutes

val scala3Version = "3.5.2"

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .in(file("core"))
  .settings(
    name := "wobblelab-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions ++= List("-Xcheck-macros", "-Xmax-inlines", "300"),
    libraryDependencies += "org.typelevel" %%% "cats-parse" % "1.0.0",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0" % Test,
    libraryDependencies += "org.typelevel" %%% "cats-core" % "2.12.0",
    libraryDependencies ++= Seq (
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser",
    ).map(_ % "0.14.1"),
    libraryDependencies += "io.circe" %%% "circe-yaml-scalayaml" % "0.16.0",
    Compile / sourceGenerators += Def.task {
      val daFile = (Compile / sourceManaged).value / "wobblelab" / "doggenev12.scala"
      val data = Files.readString(file("assets/genes_v12.yaml").toPath)
      IO.write(daFile,
        s"""
          |package net.bulbyvr
          |package wobblelab
          |
          |val GeneV12_yaml: String =
          |\"\"\"
          |$data
          |\"\"\"
          |""".stripMargin
      )
      Seq(daFile)
    }.taskValue
  ).jsSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-locales" % "1.2.0"
  )
ThisBuild / resolvers +=
  "Sonatype OSS Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots"

lazy val swingio = project
  .dependsOn(core.jvm)
  .in(file("swingio"))
  .settings(
    name := "wobblelab-swing-io",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
      scalacOptions ++= List("-Xcheck-macros"),
    libraryDependencies += "io.github.thedrawingcoder-gamer" %% "swing-io" % "0.1-f0bb06b-SNAPSHOT",

    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4"
  )
val webTarget = settingKey[File]("webTarget")
val tearDownWeb = TaskKey[Unit]("tearDownWeb")
val linkJSDir = TaskKey[File]("linkJSDir")
val fastCopyWeb = TaskKey[Unit]("fastCopyWeb")
val fullCopyWeb = TaskKey[Unit]("fullCopyWeb")
val serve = TaskKey[Unit]("serve")

val fastBuild = TaskKey[Unit]("fastBuild")
val fullBuild = TaskKey[Unit]("fullBuild")

def routes(path: String): HttpRoutes[CIO] = {
  fileService[CIO](FileService.Config[CIO](path))
}
def servePage(dir: File): CIO[Nothing] = {
  val port = Port.fromInt(8000).get
  val bind = IpAddress.fromString("127.0.0.1").get
  EmberServerBuilder
    .default[CIO]
    .withHost(bind)
    .withPort(port)
    .withHttpApp(routes(dir.toPath.toString).orNotFound)
    .build
    .evalTap(s =>
      CIO.println(
        s"Serving HTTP on ${s.addressIp4s.host} port ${s.addressIp4s.port} (http://${s.addressIp4s}/)"
      )
    )
    .useForever
}


def copyImpl(linkDir: File, targetDir: File, baseDir: File) = {
  if (!Files.exists(targetDir.toPath))
    IO.createDirectory(targetDir)
  IO.copyDirectory(linkDir, targetDir)
  IO.copyDirectory(file("assets"), new File(targetDir, "resources"))
  IO.copyDirectory(new File(baseDir, "web"), targetDir)
}

def tearDownImpl(targetDir: File) = {
  if (Files.exists(targetDir.toPath))
    IO.delete(targetDir)
}

import cats.effect.unsafe.implicits.global
lazy val web = project
  .enablePlugins(ScalaJSPlugin)
  .in(file("web"))
  .dependsOn(core.js)
  .settings(
    name := "wobblelab-web",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    libraryDependencies += "com.armanbilge" %%% "calico"% "0.2.2-95-60c8853-SNAPSHOT",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",
    scalaJSUseMainModuleInitializer := true,
    webTarget := new File((Compile / target).value, "webpage"),
    Compile / fullLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    fastCopyWeb := copyImpl((Compile / fastLinkJSOutput).value, webTarget.value, baseDirectory.value),
    fullCopyWeb := copyImpl((Compile / fullLinkJSOutput).value, webTarget.value, baseDirectory.value),
    tearDownWeb := tearDownImpl(webTarget.value),
    fastBuild := {
      Def.sequential(
        tearDownWeb,
        Compile / fastLinkJS,
        fastCopyWeb
      ).value
    },
    fullBuild := {
      Def.sequential(
        tearDownWeb,
        Compile / fullLinkJS,
        fullCopyWeb
      ).value
    },
    serve := {
      fastBuild.value
      servePage(webTarget.value).unsafeRunSync()
    }


  )
lazy val root = project.aggregate(core.jvm, swingio)