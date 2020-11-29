import Dependencies._

ThisBuild / scalaVersion := "2.13.4"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .aggregate(
    `advent-of-code-2015`,
    `advent-of-code-2018`,
    `advent-of-code-2019`,
    `advent-of-code-2020`,
    `benchmark-2018`
  )
  .settings(
    name := "advent-of-code"
  )

lazy val `advent-of-code-2015` = (project in file("2015"))
  .dependsOn(core).settings(
    wartremoverErrors ++= Warts.unsafe.diff(Seq(Wart.Var, Wart.StringPlusAny)),
    libraryDependencies += scalaTest % Test
  )

lazy val `advent-of-code-2018` = (project in file("2018"))
  .settings(
    scalaVersion := "2.12.10",
    scalacOptions --= Seq("-Xfatal-warnings"),
    libraryDependencies += scalaTest308 % Test
  )

lazy val `advent-of-code-2019` = (project in file("2019"))
  .dependsOn(core).settings(
    libraryDependencies += scalaTest308 % Test
  )

lazy val `advent-of-code-2020` = (project in file("2020"))
  .dependsOn(core).settings(
    wartremoverErrors ++= Warts.unsafe.diff(Seq(Wart.Var, Wart.StringPlusAny)),
    libraryDependencies += scalaTest % Test
  )

lazy val `core` = (project in file("core")).settings(
  libraryDependencies += scalaTest % Test
)

lazy val `benchmark-2018` = project
  .enablePlugins(JmhPlugin)
  .settings(
    scalaVersion := "2.12.10"
  )

//lazy val root = (project in file("."))
//  .settings(
//    name := "advent-of-code",
//    libraryDependencies += scalaTest % Test
//  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
