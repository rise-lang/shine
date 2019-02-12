name := "idealised-OpenCL"

version := "1.0"

scalaVersion := "2.11.12"

scalacOptions ++= Seq("-Xmax-classfile-name", "100", "-unchecked", "-deprecation", "-feature")

fork := true
javaOptions += "-Djava.library.path=lib/lift/lib/Executor/build"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)


// Scala libraries
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.12"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.12"
libraryDependencies += "org.scala-lang" % "scala-library" % "2.11.12"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.3.10"

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.4"

libraryDependencies += "jline" % "jline" % "2.12.1"

// JUnit
libraryDependencies += "junit" % "junit" % "4.11"
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

// ScalaCheck
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"

// Scalatest
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

// TODO: Pick one for argument parsing
libraryDependencies += "commons-cli" % "commons-cli" % "1.3.1"
libraryDependencies += "org.clapper" %% "argot" % "1.0.3"

// Logging
libraryDependencies += "ch.qos.logback" %  "logback-classic" % "1.1.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

// Build ArithExpr
unmanagedSourceDirectories in Compile += baseDirectory.value / "lib/ArithExpr/src/main/"
unmanagedSourceDirectories in Test += baseDirectory.value / "lib/ArithExpr/src/main/"

// Build lift
unmanagedSourceDirectories in Compile += baseDirectory.value / "lib/lift/src/main/"
unmanagedSourceDirectories in Test += baseDirectory.value / "lib/lift/src/main/"
