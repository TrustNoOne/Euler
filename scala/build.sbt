scalaVersion := "2.13.6"

scalacOptions ++= Seq("-Ymacro-annotations")

Compile / doc / sources := Seq.empty

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
)

lazy val `project-euler` = project in file(".")
