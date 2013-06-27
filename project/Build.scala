import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
import EclipseKeys._

object BuildSettings {
  val buildOrganization = "trustno1"
  val buildVersion = "1.0.0"
  val buildScalaVersion = "2.10.2"

  val sbteclipseSettings = Seq(
    withSource := true,
    createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource)

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion)
}

object EulerBuild extends Build {
  import BuildSettings._

  lazy val root = Project(
    "ProjectEuler",
    file("."),
    settings = buildSettings ++ sbteclipseSettings)

}

