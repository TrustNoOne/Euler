version := "1.0-SNAPSHOT"
scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
 "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

scalacOptions ++= Seq("-encoding", "UTF-8", "-target:jvm-1.8", "-deprecation", "-feature", "-unchecked", "-Xlog-reflective-calls",
  "-Xlint:by-name-right-associative", "-Xlint:delayedinit-select", "-Xlint:doc-detached", "-Xlint:inaccessible", "-Xlint:infer-any",
  "-Xlint:missing-interpolator", "-Xlint:nullary-override", "-Xlint:nullary-unit", "-Xlint:option-implicit", "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload", "-Xlint:private-shadow", "-Xlint:unsound-match", "-Ywarn-adapted-args")
javacOptions  ++= Seq("-source", "8", "-target", "8", "-Xlint:unchecked", "-Xlint:deprecation")

lazy val `project-euler` = project in file(".")
