scalaSource in Compile := baseDirectory.value / "src"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint", "-language:higherKinds")

fork in run := true
cancelable in Global := true
