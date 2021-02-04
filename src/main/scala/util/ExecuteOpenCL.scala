package util

import scala.language.postfixOps
import scala.sys.process._

object ExecuteOpenCL {
  case class Exception(msg: String) extends Throwable

  //noinspection ScalaUnnecessaryParentheses
  @throws[Exception]
  def apply(module: shine.OpenCL.Module, buffer_impl: String, mainSource: String): String = {
    val genDir = java.nio.file.Files.createTempDirectory("shine-gen").toFile();
    try {
      val binPath = s"${genDir.getAbsolutePath}/main"
      val mainPath = s"${genDir.getAbsolutePath}/main.c"
      val hostPath = s"${genDir.getAbsolutePath}/host.c"
      writeToPath(mainPath, mainSource)
      writeToPath(hostPath, gen.c.function.asString(module.host))
      module.kernels.foreach { km =>
        // assumes a single kernel per module
        val fileName = km.kernels(0).code.name
        writeToPath(s"${genDir.getAbsolutePath}/$fileName.cl",
          gen.opencl.kernel.asString(km))
      }
      // host.c is directly included in the main
      (s"clang -O2 $mainPath data/runtime/buffer_${buffer_impl}.c data/runtime/ocl.c -I data/runtime/ -o $binPath -lm -lOpenCL -Wno-parentheses-equality" !!)
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
}

