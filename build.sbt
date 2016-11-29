name := "XMaths-lib"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

dependencyOverrides += "org.scala-lang" % "scala-compiler" % scalaVersion.value