import sbt.Keys._
import sbt._

object ScalaUtilsBuild extends Build {
  val Organization = "fi.vm.sade"
  val Name = "scala-utils"
  val Version = "0.1.0-SNAPSHOT"
  val JavaVersion = "1.7"
  val ScalaVersion = "2.11.1"
  val TomcatVersion = "7.0.22"
  val artifactory = "https://artifactory.oph.ware.fi/artifactory"

  if(!System.getProperty("java.version").startsWith(JavaVersion)) {
    throw new IllegalStateException("Wrong java version (required " + JavaVersion + "): " + System.getProperty("java.version"))
  }

  lazy val project = Project (
    "scala-utils",
    file("."),
    settings = Defaults.coreDefaultSettings
      ++ Seq(
      organization := Organization,
      name := Name,
      version := Version,
      scalaVersion := ScalaVersion,
      javacOptions ++= Seq("-source", JavaVersion, "-target", JavaVersion),
      scalacOptions ++= Seq("-target:jvm-1.7", "-deprecation"),
      resolvers += Resolver.mavenLocal,
      resolvers += Classpaths.typesafeReleases,
      resolvers += "oph-sade-artifactory-releases" at artifactory + "/oph-sade-release-local",
      resolvers += "oph-sade-artifactory-snapshots" at artifactory + "/oph-sade-snapshot-local",
      parallelExecution in Test := false,
      libraryDependencies ++= Seq(
        "org.slf4j" % "slf4j-api" % "1.7.7",
        "org.slf4j" % "slf4j-log4j12" %"1.7.6",
        "com.typesafe" % "config" % "1.2.1",
        "org.json4s" %% "json4s-jackson" % "3.2.10",
        "org.json4s" %% "json4s-ext" % "3.2.10",
        "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.4.1",
        "org.scalatra.scalate" %% "scalate-core" % "1.7.0",
        "org.scalaj" %% "scalaj-http" % "1.1.0",
        "de.flapdoodle.embed" % "de.flapdoodle.embed.mongo" % "1.46.0",
        "org.specs2" %% "specs2" % "2.3.12" % "test"
      ),
      credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
      publishTo := {
        if (Version.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at artifactory + "/oph-sade-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
        else
          Some("releases" at artifactory + "/oph-sade-release-local")
      }
    )
  ).settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*)
  lazy val projectRef: ProjectReference = project

}
