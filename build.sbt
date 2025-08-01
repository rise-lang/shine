ThisBuild / scalaVersion := "2.13.16"
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
  .aggregate(executor, CUexecutor)
  .dependsOn(meta, arithExpr, executor, CUexecutor, elevate)
  .settings(
    name          := "riseAndShine",
    version       := "1.0",

    javaOptions ++= Seq("-Djava.library.path=lib/yacx/build:lib/executor/lib/Executor/build",
      "-XX:+HeapDumpOnOutOfMemoryError", "-XX:HeapDumpPath=/tmp/rise-and-shine.hprof",
      "-DexecuteCudaTests=false", "-Xss26m", "-Xmx2048m"),

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
        "org.apache.logging.log4j" % "log4j-core" % "2.14.1",
        "org.apache.logging.log4j" %% "log4j-api-scala" % "12.0",
        // json
        "com.typesafe.play" %% "play-json" % "2.9.1",
        // subprocess communication
        "com.lihaoyi" %% "os-lib" % "0.7.3"
    ),

    compile := ((compile in Compile) dependsOn (generateRISEPrimitives, clap)).value,
    test    := ((test in Test) dependsOn (generateRISEPrimitives, clap)).value
  )

lazy val generateRISEPrimitives = taskKey[Unit]("Generate RISE Primitives")

generateRISEPrimitives := {
  runner.value.run("meta.generator.RisePrimitives",
    (dependencyClasspath in Compile).value.files,
    Seq((scalaSource in Compile).value.getAbsolutePath),
    streams.value.log).failed foreach (sys error _.getMessage)
}

lazy val generateDPIAPrimitives = taskKey[Unit]("Generate DPIA Primitives")

generateDPIAPrimitives := {
  runner.value.run("meta.generator.DPIAPrimitives",
    (dependencyClasspath in Compile).value.files,
    Seq((scalaSource in Compile).value.getAbsolutePath),
    streams.value.log).failed foreach (sys error _.getMessage)
}

lazy val meta = (project in file("meta"))
  .settings(
    name := "meta",
    version := "1.0",
    commonSettings,
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2",
    libraryDependencies += "com.lihaoyi" %% "scalaparse" % "2.2.2",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.3",
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.4.10",
  )

lazy val arithExpr  = project in file("lib/arithexpr")

lazy val executor   = project in file("lib/executor")

lazy val CUexecutor = project in file("lib/yacx")

lazy val elevate    = project in file("lib/elevate")

lazy val docs = (project in file("riseAndShine-docs"))
  .settings(
    moduleName := "riseAndShine-docs",
    mdocOut := file("docs-website/docs"),
  )
  .enablePlugins(MdocPlugin)
  .dependsOn(riseAndShine)

lazy val clap = taskKey[Unit]("Builds Clap library")

clap := {
  import scala.language.postfixOps
  import scala.sys.process._
  //noinspection PostfixMethodCall
  "echo y" #| (baseDirectory.value + "/lib/clap/buildClap.sh") !
}

lazy val float_safe_optimizer = (project in file("float-safe-optimizer"))
  .dependsOn(riseAndShine)
  .enablePlugins(AssemblyPlugin)
  .settings(
    excludeDependencies ++= Seq(
      ExclusionRule("org.scala-lang.modules", s"scala-xml_${scalaBinaryVersion.value}"),
      ExclusionRule("junit", "junit"),
      ExclusionRule("com.novocode", "junit-interface"),
      ExclusionRule("org.scalacheck", "scalacheck"),
      ExclusionRule("org.scalatest", "scalatest"),
      ExclusionRule("com.lihaoyi", s"os-lib_${scalaBinaryVersion.value}"),
      ExclusionRule("com.typesafe.play", s"play-json_${scalaBinaryVersion.value}"),
      ExclusionRule("org.rise-lang", s"opencl-executor_${scalaBinaryVersion.value}"),
      ExclusionRule("org.rise-lang", "CUexecutor"),
      ExclusionRule("org.elevate-lang", s"cuda-executor_${scalaBinaryVersion.value}"),
      ExclusionRule("org.elevate-lang", s"meta_${scalaBinaryVersion.value}"),
    ),
    name := "float-safe-optimizer",
    javaOptions ++= Seq("-Xss20m", "-Xms512m", "-Xmx4G"),
    assemblyOutputPath in assembly := file("float-safe-optimizer.jar"),
  )

