ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "zio-jackson-tokens",
    crossScalaVersions := Seq("2.12.13", "3.3.1"),
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.16.1",
      "org.json4s" %% "json4s-core" % "4.0.7",
      "dev.zio" %% "zio-streams" % "2.0.21" % Optional,
      "dev.zio" %% "zio-streams" % "2.0.21" % Test,
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
      "org.json4s" %% "json4s-jackson" % "4.0.7" % Test
    )
  )
