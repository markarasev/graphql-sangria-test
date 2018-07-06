
name := "graphql-sangria-test"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.sangria-graphql" %% "sangria" % "1.4.1",
  "org.sangria-graphql" %% "sangria-play-json" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)
