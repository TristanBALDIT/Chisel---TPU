name := "Chisel-TPU"
version := "0.1.0"

scalaVersion := "2.13.16"
val chiselVersion = "6.7.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  // "-Xfatal-warnings",
  "-language:reflectiveCalls"
)

addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.chipsalliance" %% "chisel"     % chiselVersion,
  "edu.berkeley.cs"   %% "chiseltest" % "6.0.0" % Test
)
