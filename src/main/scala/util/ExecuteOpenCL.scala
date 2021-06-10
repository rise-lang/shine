package util

import rise.autotune.{AutoTuningError, COMPILATION_ERROR, EXECUTION_ERROR, NO_ERROR}

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

    // convert input String to array of Strings (line-wise)
    val sArray = s.split("\n")

    // check weird error, where the first line has empty element tag
    // avoid this error by removing first line
    val sCorrect = sArray(0).trim().takeRight(2) match {
      case "/>" => sArray.drop(1).mkString("\n")
      case _ => s
    }

    // get xml form string
    val clapResult = scala.xml.XML.loadString(sCorrect)

    // get start and end time
    val start = (clapResult \\ "@start").toString().toLong
    val end = (clapResult \\ "@end").toString().toLong
    val runtime = end - start

    // convert to ms
    TimeSpan.inMilliseconds(runtime.toDouble/1000000)
  }

  def executeWithRuntime(code: String, buffer_impl: String): (Option[TimeSpan[Time.ms]], AutoTuningError) = {
    val src = writeToTempFile("code-", ".c", code).getAbsolutePath
    val bin = createTempFile("bin-", "").getAbsolutePath
    val sources = s"$src $runtimePath/buffer_$buffer_impl.c $runtimePath/ocl.c"
    try {
      //        (s"clang -O2 $sources $includes -o $bin $libDirs $libs -Wno-parentheses-equality" !!)
      (s"timeout 5s clang -O2 $sources $includes -o $bin $libDirs $libs -Wno-parentheses-equality" !!)
    } catch {
      case e:Throwable => {
        println("compile error: " + e)
        //          throw Exception("COMPILATION_ERROR")

        (None, AutoTuningError(COMPILATION_ERROR, Some(e.toString)))
      }
    }
    try{
      val result = (s"timeout 5s runtime/clap_wrapper.sh $bin" !!)
//      println("result: " + result)
      val runtime = getRuntimeFromClap(result)

      (Some(runtime), AutoTuningError(NO_ERROR, None))
    } catch {
      // todo check error codes here
      case e: Throwable => {
        println("execution error: " + e)

        (None, AutoTuningError(EXECUTION_ERROR, Some(e.toString)))
      }
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

