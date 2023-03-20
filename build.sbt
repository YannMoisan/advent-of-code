import Dependencies._

ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .aggregate(
    `advent-of-code-2015`,
    `advent-of-code-2016`,
    `advent-of-code-2017`,
    `advent-of-code-2018`,
    `advent-of-code-2019`,
    `advent-of-code-2020`,
    `advent-of-code-2021`,
    `advent-of-code-2022`,
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

val monocleVersion = "2.0.5"

lazy val `advent-of-code-2016` = (project in file("2016"))
  .dependsOn(core)
  .settings(
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies += scalaTest % Test,
    // Change this to another test framework if you prefer
    libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-law"   % monocleVersion % "test"
    )
  )

lazy val `advent-of-code-2017` = (project in file("2017"))
  .dependsOn(core)
  .settings(
    libraryDependencies += scalaTest % Test
  )

lazy val `advent-of-code-2018` = (project in file("2018"))
  .dependsOn(core)
  .settings(
    libraryDependencies += scalaTest % Test
  )

lazy val `advent-of-code-2019` = (project in file("2019"))
  .dependsOn(core).settings(
    libraryDependencies += scalaTest308 % Test
  )

lazy val `advent-of-code-2020` = (project in file("2020"))
  .dependsOn(core).settings(
    wartremoverErrors ++= Warts.unsafe.diff(Seq(Wart.Var, Wart.StringPlusAny)),
    libraryDependencies += scalaTest     % Test,
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2"
  )

lazy val `advent-of-code-2021` = (project in file("2021"))
  .dependsOn(core).settings(
    wartremoverErrors ++= Warts.unsafe.diff(Seq(Wart.Var, Wart.StringPlusAny)),
    libraryDependencies += scalaTest % Test
  )

lazy val `advent-of-code-2022` = (project in file("2022"))
  .dependsOn(core).settings(
    wartremoverErrors ++= Warts.unsafe.diff(Seq(Wart.Var, Wart.StringPlusAny)),
    libraryDependencies += scalaTest % Test
  )

lazy val `core` = (project in file("core")).settings(
  libraryDependencies += scalaTest,
  libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.10"
)

lazy val `benchmark-2018` = project
  .enablePlugins(JmhPlugin)
  .settings(
    ).dependsOn(core, `advent-of-code-2021`)
//lazy val root = (project in file("."))
//  .settings(
//    name := "advent-of-code",
//    libraryDependencies += scalaTest % Test
//  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
