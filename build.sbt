name := "matching-complexity"
scalaVersion := "2.12.8"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")
scalacOptions ++= Seq("-language:higherKinds")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

shellPrompt := {_ => s"${scala.Console.MAGENTA}sbt:${name.value}> ${scala.Console.RESET}"}
autoStartServer := false
fork in run := true
connectInput in run := true
cancelable in Global := true
