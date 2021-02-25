package util

import scala.language.postfixOps
import scala.sys.process._

object ExecuteOpenCL {
  case class Exception(msg: String) extends Throwable

  val clHeaderPath = "lib/executor/lib/Executor/include/"
  val runtimePath = "runtime/"
  val executorHeadersPath = "lib/executor/lib/Executor/include/"
  val libs = "-lm -lOpenCL"
  val includes = s"-I$runtimePath -I$executorHeadersPath"

  // noinspection ScalaUnnecessaryParentheses
  @throws[Exception]
  def usingDirectory(module: shine.OpenCL.Module,
                     buffer_impl: String,
                     mainSource: String): String = {
    val genDir = java.nio.file.Files.createTempDirectory("shine-gen").toFile;
    try {
      shine.OpenCL.Module.dumpToDirectory(genDir)(module)
      val binPath = s"${genDir.getAbsolutePath}/main"
      val mainPath = s"${genDir.getAbsolutePath}/main.c"
      writeToPath(mainPath,
        s"""#include "host.c"
           |${mainSource}""".stripMargin)
      val sources = s"$mainPath $runtimePath/buffer_${buffer_impl}.c $runtimePath/ocl.c"
      (s"clang -O2 $sources -I $runtimePath -I $clHeaderPath -o $binPath $libs -Wno-parentheses-equality" !!)
      (Process(s"$binPath", new java.io.File(genDir.getAbsolutePath)) !!)
    } catch {
      case e: Throwable =>
        Console.err.println(s"execution failed: $e")
        throw Exception(s"execution failed: $e")
    } finally {
      new scala.reflect.io.Directory(new java.io.File(genDir.getAbsolutePath))
          .deleteRecursively()
    }
  }

  @throws[Exception]
  def apply(code: String, buffer_impl: String): String = {
    try {
      val src = writeToTempFile("code-", ".c", code).getAbsolutePath
      val bin = createTempFile("bin-", "").getAbsolutePath
      val sources = s"$src $runtimePath/buffer_${buffer_impl}.c $runtimePath/ocl.c"
      (s"clang -O2 $sources -I $runtimePath -I $clHeaderPath -o $bin $libs -Wno-parentheses-equality" !!)
      (s"$bin" !!)
    } catch {
      case e: Throwable =>
        Console.err.println(s"execution failed: $e")
        throw Exception(s"execution failed: $e")
    }
  }
}

