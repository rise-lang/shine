ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "org.rise-lang"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-Wunused:nowarn",
    "-Xfatal-warnings",
    "-Xlint:-unused",
    "-Ymacro-annotations",
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:reflectiveCalls",
  ),
  fork := true
)

lazy val riseAndShine = (project in file("."))
  .aggregate(executor)
  .dependsOn(riseAndShineMacros, arithExpr, executor, elevate)
  .settings(
    name          := "riseAndShine",
    version       := "1.0",

    javaOptions ++= Seq("-Djava.library.path=lib/executor/lib/Executor/build", "-Xss26m"),

    commonSettings,

    libraryDependencies ++= Seq(
        // scala
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.scala-lang" % "scala-library" % scalaVersion.value,
        "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
        "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
        // testing
        "junit" % "junit" % "4.11",
        "org.scalatest" %% "scalatest" % "3.1.0" % "test",
        // json
        "com.typesafe.play" %% "play-json" % "2.9.1"
    )
  )

lazy val meta = (project in file("meta"))
  .settings(
    name := "meta",
    version := "1.0",
    commonSettings,
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.3",
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.4.10",
  )

lazy val riseAndShineMacros = (project in file("macros"))
  .settings(
    name := "riseAndShineMacros",
    version := "1.0",
    commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val arithExpr = (project in file("lib/arithexpr"))

lazy val executor   = (project in file("lib/executor"))

lazy val elevate    = (project in file("lib/elevate"))
