name := """fpinscala exercises"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file("."))

scalaVersion := "2.11.8"


libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test")

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
//routesGenerator := InjectedRoutesGenerator