ThisBuild / name := "functionals"
ThisBuild / version := "0.1"
ThisBuild / scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest"  % "scalatest_2.12" % "3.0.5"  % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck"    % "1.14.0" % "test"

scalacOptions in ThisBuild ++= Seq(
  "-language:_",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)
