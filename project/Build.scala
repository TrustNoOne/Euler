import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
import EclipseKeys._

object BuildSettings {
  val buildOrganization = "trustno1"
  val buildVersion = "1.0.0"
  val buildScalaVersion = "2.11.2"

  val sbteclipseSettings = Seq(
    withSource := true,
    createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource)

  val buildSettings = Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq("-feature", "-deprecation"))
}

object Dependencies {
  val scalaReflection = "org.scala-lang" % "scala-reflect" % BuildSettings.buildScalaVersion

  val compileDeps = Seq(scalaReflection)
}

object EulerBuild extends Build {
  import BuildSettings._

  lazy val root = Project(
    "ProjectEuler",
    file("."),
    settings = buildSettings ++
      sbteclipseSettings ++
      Seq(libraryDependencies ++= Dependencies.compileDeps))

}

