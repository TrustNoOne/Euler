name := """project-euler"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
 "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

scalacOptions := Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-deprecation",
  "-encoding", "utf8",
  "-Ywarn-adapted-args"
)
