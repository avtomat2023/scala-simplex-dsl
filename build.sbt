lazy val root = (project in file(".")).settings(
  name := """scala-simplex-dsl""",
  version := "0.1",

  scalaSource in Compile := baseDirectory.value / "main",
  scalaSource in Test := baseDirectory.value / "test",

  scalaVersion := "2.11.7",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),

  libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
  )
)
