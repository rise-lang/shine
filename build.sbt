lazy val riseAndShine = (project in file("."))
  .aggregate(meta, executor, CUexecutor)
  .dependsOn(arithExpr, executor, CUexecutor, elevate)
  .settings(
    name          := "riseAndShine",
    organization := "org.rise-lang",
    version       := "1.0",
    scalaVersion := "3.0.1",

    javaOptions ++= Seq("-Djava.library.path=lib/yacx/build:lib/executor/lib/Executor/build",
      "-DexecuteCudaTests=false", "-Xss26m"),

    scalacOptions ++= Seq(
      //      "-Xfatal-warnings",
      //      "-rewrite",
      "-source:3.0-migration",
      //      "-indent",
      //      "-new-syntax",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:reflectiveCalls",
    ),

    fork := true,

    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3",
      // testing
      "junit" % "junit" % "4.11",
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
      "org.apache.logging.log4j" % "log4j-core" % "2.14.1",
      "org.wvlet.airframe" %% "airframe-log" % "21.5.4",
      // os
      ("com.lihaoyi" %% "os-lib" % "0.7.8").cross(CrossVersion.for3Use2_13),
      // json
      ("com.typesafe.play" %% "play-json" % "2.9.2").cross(CrossVersion.for3Use2_13),
      // xml
      "org.scala-lang.modules" %% "scala-xml" % "2.0.1"
    ),

    compile := ((Compile / compile) dependsOn generateRISEPrimitives).value,
    test    := ((Test / test) dependsOn generateRISEPrimitives).value
  )

lazy val generateRISEPrimitives = taskKey[Unit]("Generate RISE Primitives")

generateRISEPrimitives := (Def.taskDyn {
  (meta / Compile / runMain).toTask(
    " meta.generator.RisePrimitives " + (Compile / scalaSource).value.getAbsolutePath
  )
}).value

lazy val generateDPIAPrimitives = taskKey[Unit]("Generate DPIA Primitives")

generateDPIAPrimitives := (Def.taskDyn {
  (meta / Compile / runMain).toTask(
    " meta.generator.DPIAPrimitives " + (Compile / scalaSource).value.getAbsolutePath
  )
}).value

lazy val meta       = (project in file("meta"))

lazy val arithExpr  = project in file("lib/arithexpr")

lazy val executor   = project in file("lib/executor")

lazy val CUexecutor = project in file("lib/yacx")

lazy val elevate    = project in file("lib/elevate")

lazy val docs       = (project in file("riseAndShine-docs"))
  .settings(
    moduleName := "riseAndShine-docs",
    mdocOut := file("docs-website/docs"),
    scalaVersion := "3.0.0",
  )
  .enablePlugins(MdocPlugin)
  .dependsOn(riseAndShine)
