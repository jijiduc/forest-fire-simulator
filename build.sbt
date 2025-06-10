ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.7.0"

lazy val root = (project in file("."))
  .settings(
    name := "forest-fire-simulator",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.19",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % "test",
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.typelevel" %% "cats-effect" % "3.5.0",
      "co.fs2" %% "fs2-core" % "3.9.0",
      // JSON serialization
      "io.circe" %% "circe-core" % "0.14.9",
      "io.circe" %% "circe-generic" % "0.14.9",
      "io.circe" %% "circe-parser" % "0.14.9",
      // HTTP server for streaming
      "org.http4s" %% "http4s-dsl" % "0.23.23",
      "org.http4s" %% "http4s-ember-server" % "0.23.23",
      "org.http4s" %% "http4s-ember-client" % "0.23.23",
      "org.http4s" %% "http4s-circe" % "0.23.23"
    )
  )
