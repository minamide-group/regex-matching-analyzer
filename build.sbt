libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")
scalacOptions ++= Seq("-language:higherKinds", "-language:existentials")

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"

fork in run := true
cancelable in Global := true
