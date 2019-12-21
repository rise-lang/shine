ThisBuild / scalaVersion := "2.11.12"
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
  .dependsOn(macroSub, arithExpr)
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
        "org.scalatest" %% "scalatest" % "3.0.5" % "test",
        // Silencer: Scala compiler plugin for warning suppression
        compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.4.0"),
        "com.github.ghik" %% "silencer-lib" % "1.4.0" % Provided
    )
  )

lazy val macroSub = (project in file("macros"))
  .settings(
    name := "macros",
    version := "1.0",
    commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val arithExpr = (project in file("lib/arithexpr"))