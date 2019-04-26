name := "idealised-OpenCL"

version := "1.0"

scalaVersion := "2.11.12"

compile := ((compile in Compile) dependsOn setup).value
test := ((test in Test) dependsOn setup).value

lazy val setup = taskKey[Unit]("Sets up the submodules")

setup := {
  import scala.language.postfixOps
  import scala.sys.process._
  //noinspection PostfixMethodCall
  "echo y" #| "./setup.sh" !
}


scalacOptions ++= Seq("-Xmax-classfile-name", "100", "-unchecked", "-deprecation", "-feature")

fork := true
javaOptions += "-Djava.library.path=lib/executor/lib/Executor/build"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)


// Scala libraries
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.12"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.12"
libraryDependencies += "org.scala-lang" % "scala-library" % "2.11.12"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"

// JUnit
libraryDependencies += "junit" % "junit" % "4.11"

// ScalaCheck
// libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"

// Scalatest
// libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

// Build ArithExpr
unmanagedSourceDirectories in Compile += baseDirectory.value / "lib/ArithExpr/src/main/"
unmanagedSourceDirectories in Test += baseDirectory.value / "lib/ArithExpr/src/main/"

// Build executor
unmanagedSourceDirectories in Compile += baseDirectory.value / "lib/executor/src/main/"
unmanagedSourceDirectories in Test += baseDirectory.value / "lib/executor/src/main/"
