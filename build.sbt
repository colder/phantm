name := "phantm"

version := "1.0.6"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "1.8"
)

mainClass in (Compile, run) := Some("phantm.Main")

parallelExecution in Test := false
