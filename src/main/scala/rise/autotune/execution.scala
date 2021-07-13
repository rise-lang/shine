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

  def execute(expression: Expr,
              hostCode: HostCode,
              timeouts: Timeouts,
              executionIterations: Int,
              speedupFactor: Double)
  : ExecutionResult = {

    val codegenStart = System.currentTimeMillis()
    val m = autoTuningUtils.runWithTimeout(
      timeouts.codegenerationTimeout)(gen.opencl.hosted("fun").fromExpr(expression)
    )
    val codegenTime = TimeSpan.inMilliseconds((System.currentTimeMillis() - codegenStart).toDouble)
    m match {
      case Some(_) => {

        val program =
          s"""
             |${shine.OpenCL.Module.translateToString(m.get)}
             |
             |int main() {
             |  assertReasonableTimeResolution();
             |  Context ctx = createDefaultContext();
             |  fun_t fun;
             |  fun_init(ctx, &fun);
             |
             |  ${hostCode.init}
             |  waitFinished(ctx);
             |
             |  for (int sample = 0; sample < ${executionIterations}; sample++) {
             |    ${hostCode.compute}
             |    waitFinished(ctx);
             |  }
             |  ${hostCode.finish}
             |  destroyContext(ctx);
             |  return EXIT_SUCCESS;
             |}
             |""".stripMargin

        // execute program
        val result = executeWithRuntime(
          program,
          "zero_copy",
          timeouts.compilationTimeout,
          timeouts.executionTimeout,
          executionIterations,
          speedupFactor
        )

        ExecutionResult(result._1, result._2, Some(codegenTime), result._3, result._4)
      }
      case None => ExecutionResult(
        None,
        AutoTuningError(
          CODE_GENERATION_ERROR,
          Some("timeout after: " + timeouts.codegenerationTimeout),
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

        val result = (s"timeout ${executionTimeout.toDouble/1000.toDouble}s runtime/clap_wrapper.sh $bin" !!(logger))
        val runtimes = getRuntimeFromClap(result)
//        println("runtimes: " + runtimes)

//        speedupCondition = best match {
//          case Some(value) => runtime(0).value > value * speedup
//          case None => false
//        }

//        runtimes += runtime(0).value
//        i += 1
//      }
//      val runtimes_sorted = runtimes.sorted
//      println("runtimes_sorted: " + runtimes_sorted)

      // get median
      val runtime = runtimes.sorted.apply(executionIterations/2)

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

  def getRuntimeFromClap(s: String): Seq[TimeSpan[Time.ms]] = {
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

    val startSeq = (clapResult \\ "@start")
    val endSeq = (clapResult \\ "@end")

    val runtimes = (startSeq zip endSeq).map(timespan => {
      TimeSpan.inMilliseconds(
        (timespan._2.toString().toLong - timespan._1.toString().toLong)
          .toDouble/1000000)
    })

    runtimes
  }
}
