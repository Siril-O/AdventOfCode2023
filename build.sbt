ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode2023"
  )

mainClass in (Compile, run) := Some("com.siril.advent.Main")

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.19.2" % "test")

