import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "br.poli.compiladores",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Dartmouth Basic",
    libraryDependencies += scalaTest % Test
  )
