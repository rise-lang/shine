ThisBuild / scalaVersion := "2.12.10"
ThisBuild / organization := "org.rise-lang"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-Xlint",
    "-Xmax-classfile-name", "100",
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:reflectiveCalls"
  ),
  fork := true,
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

lazy val rise = (project in file("."))
  .dependsOn(riseMacros, arithExpr)
  .settings(
    name          := "rise",
    version       := "1.0",

    commonSettings,

    libraryDependencies ++= Seq(
        // scala
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.scala-lang" % "scala-library" % scalaVersion.value,
        // testing
        "junit" % "junit" % "4.11",
        "org.scalatest" %% "scalatest" % "3.1.0" % "test",
        // Silencer: Scala compiler plugin for warning suppression
        compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.4.3" cross CrossVersion.full),
        "com.github.ghik" % "silencer-lib" % "1.4.3" % Provided cross CrossVersion.full
    )
  )

lazy val riseMacros = (project in file("macros"))
  .settings(
    name := "macros",
    version := "1.0",
    commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val arithExpr = (project in file("lib/arithexpr"))