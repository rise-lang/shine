package rise.autotune

import rise.core.Expr
import util.ExecuteOpenCL.{includes, libDirs, libs, platformPath}
import util.{Time, TimeSpan, createTempFile, gen, writeToTempFile}

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.sys.process._

case class ExecutionResult(runtime: Option[TimeSpan[Time.ms]],
                           error: AutoTuningError,
                           codegenTime: Option[TimeSpan[Time.ms]],
                           compilationTime: Option[TimeSpan[Time.ms]],
                           executionTime: Option[TimeSpan[Time.ms]],
                          )
object execution {
  var best:Option[Double] = None

  // logger to avoid printing of stderr
  val logger = new ProcessLogger {
    def out(s: => String): Unit = s
    def err(s: => String): Unit = s
    def buffer[T](f: => T): T = f
  }

  def execute(e: Expr,
              main: String,
              timeouts: Timeouts,
              executionIterations: Int,
              speedup: Double)
  : ExecutionResult = {

    val codegenStart = System.currentTimeMillis()
    val m = autoTuningUtils.runWithTimeout(
      timeouts.codgenerationTimeout)(gen.opencl.hosted.fromExpr(e)
    )
    val codegenTime = TimeSpan.inMilliseconds((System.currentTimeMillis() - codegenStart).toDouble)
    m match {
      case Some(_) => {
        val program = shine.OpenCL.Module.translateToString(m.get) + main

        // execute program
        val result = executeWithRuntime(
          program,
          "zero_copy",
          timeouts.compilationTimeout,
          timeouts.executionTimeout,
          executionIterations,
          speedup
        )

        ExecutionResult(result._1, result._2, Some(codegenTime), result._3, result._4)
      }
      case None => ExecutionResult(
        None,
        AutoTuningError(
          CODE_GENERATION_ERROR,
          Some("timeout after: " + timeouts.codgenerationTimeout),
        ),
        Some(codegenTime),
        None,
        None
      )
    }
  }

  def executeWithRuntime(code: String,
                         buffer_impl: String,
                         compilationTimeout: Long,
                         executionTimeout: Long,
                         executionIterations: Int,
                         speedup: Double)
  : (Option[TimeSpan[Time.ms]],
    AutoTuningError,
    Option[TimeSpan[Time.ms]],
    Option[TimeSpan[Time.ms]]) = {

    val src = writeToTempFile("code-", ".c", code).getAbsolutePath
    val bin = createTempFile("bin-", "").getAbsolutePath
    val sources = s"$src ${platformPath}buffer_$buffer_impl.c ${platformPath}ocl.c"

    val compilationStart = System.currentTimeMillis()
    try {
      (s"timeout ${compilationTimeout.toDouble/1000.toDouble}s " +
        s"clang -O2 $sources $includes -o $bin $libDirs $libs -Wno-parentheses-equality" !!)
    } catch {
      case e: Throwable => {
        val compilationTime = (System.currentTimeMillis() - compilationStart).toDouble
        (
          None,
          AutoTuningError(COMPILATION_ERROR, Some(e.toString)),
          Some(TimeSpan.inMilliseconds(compilationTime)),
          None
        )
      }
    }
    val compilationTime = TimeSpan.inMilliseconds(System.currentTimeMillis().toDouble - compilationStart)
    val executionStart = System.currentTimeMillis()
    try{
      val runtimes = ListBuffer.empty[Double]

      var i = 0
      var speedupCondition = false
      while(!speedupCondition && i < executionIterations){

        val result = (s"timeout ${executionTimeout.toDouble/1000.toDouble}s runtime/clap_wrapper.sh $bin" !!(logger))
        val runtime = getRuntimeFromClap(result)

        speedupCondition = best match {
          case Some(value) => runtime.value > value * speedup
          case None => false
        }

        runtimes += runtime.value
        i += 1
      }

      // get median
      val runtime = TimeSpan.inMilliseconds(runtimes.toSeq.sorted.apply(i/2))

      // update or init global best
      best = best match{
        case Some(value) => runtime.value < value match {
          case true => Some(runtime.value)
          case false => best
        }
        case None => Some(runtime.value)
      }

      val executionTime = TimeSpan.inMilliseconds((System.currentTimeMillis() - executionStart).toDouble)

      (Some(runtime), AutoTuningError(NO_ERROR, None), Some(compilationTime), Some(executionTime))
    } catch {
      // todo check error codes here
      case e: Throwable => {
        val executionTime = TimeSpan.inMilliseconds((System.currentTimeMillis() - executionStart).toDouble)
        (None, AutoTuningError(EXECUTION_ERROR, Some(e.toString)), Some(compilationTime), Some(executionTime))
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
