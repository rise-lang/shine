package util

import scala.language.postfixOps
import scala.sys.process._
import scala.util.Using

object ExecuteOpenCL {
  case class Exception(msg: String) extends Throwable

  val runtimePath = "runtime/"
  val executorHeadersPath = "lib/executor/lib/Executor/include/"
  val libs = "-lm -lOpenCL"
  val includes = s"-I$runtimePath -I$executorHeadersPath"
  val libDirs: String = tryToFindOpenCLLibDir()

  def tryToFindOpenCLLibDir(): String = {
    import scala.io.Source
    val cmakeCache = "lib/executor/lib/Executor/build/CMakeCache.txt"
    Using(Source.fromFile(cmakeCache)) { source =>
      source.getLines()
            .filter(_ contains "OPENCL_LIBRARIES").nextOption() match {
        case Some(line) =>
          val extractPath = raw"OPENCL_LIBRARIES:FILEPATH=(.+)/.+$$".r
          line match {
            case extractPath(path) => s"-L $path"
            case _ => ""
          }
        case None => ""
      }
    }.getOrElse("")
  }

  // noinspection ScalaUnnecessaryParentheses
  @throws[Exception]
  def usingDirectory(module: shine.OpenCL.Module,
                     buffer_impl: String,
                     mainSource: String): String = {
    val genDir = java.nio.file.Files.createTempDirectory("shine-gen").toFile
    try {
      shine.OpenCL.Module.dumpToDirectory(genDir)(module)
      val binPath = s"${genDir.getAbsolutePath}/main"
      val mainPath = s"${genDir.getAbsolutePath}/main.c"
      writeToPath(mainPath,
        s"""#include "host.c"
           |$mainSource""".stripMargin)
      val sources = s"$mainPath $runtimePath/buffer_$buffer_impl.c $runtimePath/ocl.c"
      (s"clang -O2 $sources $includes -o $binPath $libDirs $libs -Wno-parentheses-equality" !!)
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

  def getRuntimeFromClap(s: String): TimeSpan[Time.ms] = {
    // get xml form string
    val clapResult = scala.xml.XML.loadString(s)

    // get start and end time
    val start = (clapResult \\ "@start").toString().toLong
    val end = (clapResult \\ "@end").toString().toLong
    val runtime = end - start

    // convert to ms
    TimeSpan.inMilliseconds(runtime.toDouble/1000000)
  }

  @throws[Exception]
  def executeWithRuntime(code: String, buffer_impl: String): TimeSpan[Time.ms] = {
    try {
      val src = writeToTempFile("code-", ".c", code).getAbsolutePath
      val bin = createTempFile("bin-", "").getAbsolutePath
      val sources = s"$src $runtimePath/buffer_$buffer_impl.c $runtimePath/ocl.c"
      (s"clang -O2 $sources $includes -o $bin $libDirs $libs -Wno-parentheses-equality" !!)
      val result = (s"runtime/clap_wrapper.sh $bin" !!)
      getRuntimeFromClap(result)
    } catch {
      case e: Throwable =>
        Console.err.println(s"execution failed: $e")
        throw Exception(s"execution failed: $e")
    }
  }

  @throws[Exception]
  def apply(code: String, buffer_impl: String): String = {
    try {
      val src = writeToTempFile("code-", ".c", code).getAbsolutePath
      val bin = createTempFile("bin-", "").getAbsolutePath
      val sources = s"$src $runtimePath/buffer_$buffer_impl.c $runtimePath/ocl.c"
      (s"clang -O2 $sources $includes -o $bin $libDirs $libs -Wno-parentheses-equality" !!)
      (s"$bin" !!)
    } catch {
      case e: Throwable =>
        Console.err.println(s"execution failed: $e")
        throw Exception(s"execution failed: $e")
    }
  }
}

