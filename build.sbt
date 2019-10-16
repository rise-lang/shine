lazy val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.11.12",
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

lazy val root = (project in file("."))
  .dependsOn(macroSub).settings(
  commonSettings,
  name := "idealised-OpenCL",
  compile := ((compile in Compile) dependsOn setup).value,
  test := ((test in Test) dependsOn setup).value,
  javaOptions += "-Djava.library.path=lib/executor/lib/Executor/build",

  // Scala libraries
  libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.12",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.12",
  libraryDependencies += "org.scala-lang" % "scala-library" % "2.11.12",
  libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5",

  // JUnit
  libraryDependencies += "junit" % "junit" % "4.11",

  // Scalatest
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",

  // Silencer: Scala compiler plugin for warning suppression
  libraryDependencies ++= Seq(
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.4.0"),
    "com.github.ghik" %% "silencer-lib" % "1.4.0" % Provided
  ),

  // exclude the ArithExpr library sources
  scalacOptions += s"-P:silencer:pathFilters=${baseDirectory.value}/lib/ArithExpr/src/main/",

  // Build ArithExpr
  unmanagedSourceDirectories in Compile += baseDirectory.value / "lib/ArithExpr/src/main/",
  unmanagedSourceDirectories in Test += baseDirectory.value / "lib/ArithExpr/src/main/",

  // Build executor
  unmanagedSourceDirectories in Compile += baseDirectory.value / "lib/executor/src/main/",
  unmanagedSourceDirectories in Test += baseDirectory.value / "lib/executor/src/main/"
)

lazy val macroSub = (project in file("macros"))
  .settings(
    commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )