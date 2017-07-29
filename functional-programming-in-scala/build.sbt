name := "functional-programming-in-scala"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
  "junit" % "junit" % "4.10" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)