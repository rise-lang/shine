package util

import scala.language.postfixOps
import scala.sys.process._
import scala.util.Using

object ExecuteOpenCL {
  case class Exception(msg: String) extends Throwable

  val runtimePath = "runtime/"
  val platformPath = "runtime/ocl/"
  val executorHeadersPath = "lib/executor/lib/Executor/include/"
  val libs = "-lm -lOpenCL"
  val includes = s"-I$runtimePath -I$executorHeadersPath -I."
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
      val sources = s"$mainPath $platformPath/buffer_$buffer_impl.c $platformPath/ocl.c"
      (s"clang -O2 $sources $includes -o $binPath $libDirs $libs -Wno-parentheses-equality ." !!)
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
      val sources = s"$src $platformPath/buffer_$buffer_impl.c $platformPath/ocl.c"
      (s"clang -O2 $sources $includes -o $bin $libDirs $libs -Wno-parentheses-equality" !!)
      (s"$bin" !!)
    } catch {
      case e: Throwable =>
        Console.err.println(s"execution failed: $e")
        throw Exception(s"execution failed: $e")
    }
  }

  @throws[Exception]
  def using_cpp(main: String, module: shine.OpenCL.Module, buffer_impl: String): String = {
    try {
      val (m_h, m_c) = shine.OpenCL.Module.translateToHeaderAndSource(module)
      val module_hdr = writeToTempFile("code-", ".h", m_h).getAbsolutePath
      val module_src = writeToTempFile("code-", ".c", m_c).getAbsolutePath
      val main_src = writeToTempFile("code-", ".cpp", main).getAbsolutePath
      val sources = Seq(module_src, s"$platformPath/buffer_$buffer_impl.c", s"$platformPath/ocl.c")
      val objs = sources.map(s => {
        val obj = s.stripSuffix(".c") + ".o"
        if (!(new java.io.File(obj)).exists()) {
          (s"clang -c -O2 $s $includes -o $obj -Wno-parentheses-equality" !!)
        }
        obj
      }).mkString(" ")
      val bin = createTempFile("bin-", "").getAbsolutePath
      (s"clang++ -O2 $main_src $objs -include $module_hdr $includes -o $bin $libDirs $libs -Wno-parentheses-equality" !!)
      (new java.io.File(module_src.stripSuffix(".c") + ".o")).delete()
      (s"$bin" !!)
    } catch {
      case e: Throwable =>
        Console.err.println(s"execution failed: $e")
        throw Exception(s"execution failed: $e")
    }
  }
}

