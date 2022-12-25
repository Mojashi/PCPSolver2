ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

resolvers += "jitpack" at "https://jitpack.io"
//libraryDependencies += "com.github.Mojashi" % "ParikhAutomatonSolver" % "v0.3.1"

libraryDependencies += "parikhautomatonsolver"%"parikhautomatonsolver_2.13"%"0.3.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % Test

lazy val root = (project in file("."))
  .settings(
    name := "PCPSolver"
  )
