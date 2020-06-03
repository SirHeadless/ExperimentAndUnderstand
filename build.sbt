name := "ExperimentAndUnderstand"

version := "0.1"

scalaVersion := "2.12.11"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.1.3",
  "com.typesafe.akka" %% "akka-stream" % "2.6.5",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-free" % "2.0.0",


// TEST
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "com.typesafe.akka" %% "akka-http-testkit" % "10.1.3" % Test
)