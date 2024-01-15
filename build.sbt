ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "zio-jackson-tokens",
    crossScalaVersions := Seq("2.12.13", "3.3.1"),
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.16.1",
      "io.circe" %% "circe-core" % "0.14.6",
      "dev.zio" %% "zio-streams" % "2.0.21" % Optional,
      "dev.zio" %% "zio-streams" % "2.0.21" % Test,
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
      "io.circe" %% "circe-parser" % "0.14.6" % Test
    )
  )
