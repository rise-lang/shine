ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "org.lift-project"

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

lazy val setup = taskKey[Unit]("Sets up the submodules")

setup := {
  import scala.language.postfixOps
  import scala.sys.process._
  //noinspection PostfixMethodCall
  "echo y" #| "./setup.sh" !
}

lazy val shine = (project in file("."))
  .dependsOn(macroSub, arithExpr, executor)
  .settings(
    commonSettings,
    name := "idealised-OpenCL",
    version := "1.0",
    compile := ((compile in Compile) dependsOn setup).value,
    test := ((test in Test) dependsOn setup).value,
    javaOptions += "-Djava.library.path=lib/executor/lib/Executor/build",

    // Scala libraries
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value,
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5",

    // JUnit
    libraryDependencies += "junit" % "junit" % "4.11",

    // Scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",

    // Silencer: Scala compiler plugin for warning suppression
    libraryDependencies ++= Seq(
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

lazy val arithExpr = (project in file("lib/ArithExpr"))

lazy val executor  = (project in file("lib/executor"))