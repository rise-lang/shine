package rise.autotune

import rise.core.Expr
import util.ExecuteOpenCL.{includes, libDirs, libs, platformPath}
import util.{Time, TimeSpan, createTempFile, gen, writeToTempFile}
import scala.language.postfixOps
import scala.sys.process._

object execution {

  def execute(e: Expr,
              main: String,
              timeouts: Timeouts)
  : (Option[TimeSpan[Time.ms]], AutoTuningError)  = {

    val m = autoTuningUtils.runWithTimeout(
      timeouts.codgenerationTimeout)(gen.opencl.hosted.fromExpr(e)
    )
    m match {
      case Some(_) => {
        val program = shine.OpenCL.Module.translateToString(m.get) + main

        // execute program
        executeWithRuntime(
          program,
          "zero_copy",
          timeouts.compilationTimeout,
          timeouts.executionTimeout
        )
      }
      case None => (
        None,
        AutoTuningError(
          CODE_GENERATION_ERROR,
          Some("timeout after: " + timeouts.codgenerationTimeout)
        )
      )
    }
  }

  def executeWithRuntime(code: String,
                         buffer_impl: String,
                         compilationTimeout: Long, executionTimeout: Long)
  : (Option[TimeSpan[Time.ms]], AutoTuningError) = {

    val src = writeToTempFile("code-", ".c", code).getAbsolutePath
    val bin = createTempFile("bin-", "").getAbsolutePath
    val sources = s"$src ${platformPath}buffer_$buffer_impl.c ${platformPath}ocl.c"
    try {
      //scalastyle:off
      (s"timeout ${compilationTimeout/1000}s " +
        s"clang -O2 $sources $includes -o $bin $libDirs $libs -Wno-parentheses-equality" !!)
      //scalastyle:on
    } catch {
      case e: Throwable => {
        println("compile error: " + e)

        (None, AutoTuningError(COMPILATION_ERROR, Some(e.toString)))
      }
    }
    try{
      val result = (s"timeout ${executionTimeout/1000}s runtime/clap_wrapper.sh $bin" !!)
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

  def getRuntimeFromClap(s: String): TimeSpan[Time.ms] = {
    // convert input String to array of Strings (line-wise)
    val sArray = s.split("\n")

    // check for error, where the first line has empty element tag
    // avoid this error by removing first line
    val sCorrect = sArray(0).trim().takeRight(2) match {
      case "/>" => sArray.drop(1).mkString("\n")
      case _ => s
    }

    // load xml as string and compute runtime
    val clapResult = scala.xml.XML.loadString(sCorrect)
    val start = (clapResult \\ "@start").toString().toLong
    val end = (clapResult \\ "@end").toString().toLong
    val runtime = end - start

    // convert to ms
    TimeSpan.inMilliseconds(runtime.toDouble/1000000)
  }
}
