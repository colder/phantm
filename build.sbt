import AssemblyKeys._ // put this at the top of the file

name := "phantm"

version := "1.0.7"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "1.8"
)

mainClass in (Compile, run) := Some("phantm.Main")

parallelExecution in Test := false

assemblySettings

mainClass in assembly := Some("phantm.Main")
