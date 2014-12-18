import sbt.Keys._
import sbt._

object ScalaUtilsBuild extends Build {
  val Organization = "fi.vm.sade"
  val Name = "scala-utils"
  val Version = "0.1.0-SNAPSHOT"
  val JavaVersion = "1.7"
  val ScalaVersion = "2.11.1"
  val TomcatVersion = "7.0.22"

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
      resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository",
      resolvers += Classpaths.typesafeReleases,
      resolvers += "oph-sade-artifactory-releases" at "http://penaali.hard.ware.fi/artifactory/oph-sade-release-local",
      resolvers += "oph-sade-artifactory-snapshots" at "http://penaali.hard.ware.fi/artifactory/oph-sade-snapshot-local",
      parallelExecution in Test := false,
      libraryDependencies ++= Seq(
        "org.slf4j" % "slf4j-api" % "1.7.7",
        "org.scalaj" %% "scalaj-http" % "0.3.15"
      ),
      credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
      publishTo := {
        val artifactory = "http://penaali.hard.ware.fi/artifactory"
        if (Version.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at artifactory + "/oph-sade-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
        else
          Some("releases" at artifactory + "/oph-sade-release-local")
      }
    )
  ).settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*)
  lazy val projectRef: ProjectReference = project

}
