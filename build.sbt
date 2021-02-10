name := "komb"

version := "1.0.0"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalatest" %% "scalatest" % "3.3.0-SNAP3" % Test
)

resolvers ++= Seq("Maven" at "https://repo1.maven.org/maven2/")

ThisBuild / useCoursier := false