organization := "com.pidorashque"

name := "ApachanParser"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "net.sourceforge.htmlunit" % "htmlunit" % "2.15",
  "org.mapdb" % "mapdb" % "1.0.5",
  "joda-time" % "joda-time" % "2.4",
  "org.jsoup" % "jsoup" % "1.7.3",
  //"com.typesafe.akka" % "akka-actor_2.11" % "2.3.5"
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
)