lazy val meta = (project in file("."))
  .settings(
    name := "meta",
    version := "1.0",
    scalaVersion := "2.13.6",
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
    fork := true,
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2",
    libraryDependencies += "com.lihaoyi" %% "scalaparse" % "2.2.2",
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.8",
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.4.10",
  )
