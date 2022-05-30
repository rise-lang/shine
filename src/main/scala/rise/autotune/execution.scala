package rise.autotune

import rise.core.Expr
import util.ExecuteOpenCL.{includes, libDirs, libs, platformPath}
import util.{Time, TimeSpan, createTempFile, gen, writeToTempFile}

import java.util.concurrent.ExecutionException
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.sys.process._

sealed trait RuntimeStatistic
case object Median extends RuntimeStatistic
case object Minimum extends RuntimeStatistic

sealed trait FailureMode
case object `-1` extends FailureMode
case object IntMax extends FailureMode

case class ExecutionResult(runtime: Either[AutoTuningError, TimeSpan[Time.ms]],
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
              speedupFactor: Double,
              execution: RuntimeStatistic)
  : ExecutionResult = {

    val codegenStart = System.currentTimeMillis()

    val codegenResult = try {

      // run code-generation with timeout
      val codgenResult = autoTuningUtils.runWithTimeout(
        timeouts.codegenerationTimeout)(gen.opencl.hosted("fun").fromExpr(expression))

      // check if timeout was triggered
      codgenResult match {
        case Some(_) => Right(codgenResult.get)
        case None => Left(
          AutoTuningError(
            CODE_GENERATION_ERROR, Some("timeout after: " + timeouts.codegenerationTimeout)
          )
        )
      }
    } catch {
      case e:Throwable =>
        Left(AutoTuningError(CODE_GENERATION_ERROR, Some(e.getCause.getMessage)))
    }

    val codegenTime = TimeSpan.inMilliseconds((System.currentTimeMillis() - codegenStart).toDouble)

    codegenResult match {
      case Right(generatedModule) => {

        val program =
          s"""
             |#include <stdio.h>
             |#include <stdlib.h>
             |
             |${shine.OpenCL.Module.translateToString(generatedModule)}
             |
             |int main(int argc, char** argv) {
             |  Context ctx = createDefaultContext();
             |  fun_t fun;
             |  fun_init(ctx, &fun);
             |
             |  ${hostCode.init}
             |
             |  int iterations = atoi(argv[1]);
             |  for (int sample = 0; sample < iterations; sample++) {
             |    ${hostCode.compute}
             |  }
             |  ${hostCode.finish}
             |  fun_destroy(ctx, &fun);
             |  destroyContext(ctx);
             |  return EXIT_SUCCESS;
             |}
             |""".stripMargin

        println("Generated hostcode:")
        println(program)

        assert(executionIterations > 0)

        // execute program
        val result = executeWithRuntime(
          program,
          "zero_copy",
          timeouts.compilationTimeout,
          timeouts.executionTimeout,
          executionIterations,
          speedupFactor,
          execution
        )

        ExecutionResult(
          runtime = result._1,
          codegenTime = Some(codegenTime),
          compilationTime = result._2,
          executionTime = result._3)
      }
      case Left(error) =>
        ExecutionResult(
          runtime = Left(error),
          codegenTime = Some(codegenTime),
          compilationTime = None,
          executionTime = None
        )
    }
  }

  def executeWithRuntime(code: String,
                         buffer_impl: String,
                         compilationTimeout: Long,
                         executionTimeout: Long,
                         executionIterations: Int,
                         speedupFactor: Double,
                         execution: RuntimeStatistic)
  : (
    Either[AutoTuningError, TimeSpan[Time.ms]], // runtime or error
    Option[TimeSpan[Time.ms]], // compilation time
    Option[TimeSpan[Time.ms]]  // execution time
    ) = {

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
          Left(
            AutoTuningError(
              COMPILATION_ERROR,
              Some(e.toString)
            )
          ),
          Some(TimeSpan.inMilliseconds(compilationTime)),
          None
        )
      }
    }
    val compilationTime = TimeSpan.inMilliseconds(
      System.currentTimeMillis().toDouble - compilationStart
    )
    val executionStart = System.currentTimeMillis()
    try{

      // execute once to check speedup factor
      val result = (s"timeout " +
        s"${(executionTimeout*1).toDouble/1000.toDouble}s " +
        s"runtime/clap_wrapper.sh $bin 1" !!(logger))
      val runtimes = getRuntimeFromClap(result)

      // check if speedup condition is met or current best is not initialized
      val repeat = best match {
        case Some(bestValue) => runtimes(0).value < bestValue * speedupFactor
        case None => true
      }

      // repeat execution with execution iterations
      val runtime = repeat match {
        case true => {

          // repeat execution with execution iterations
          val result = (s"timeout " +
            s"${(executionTimeout*executionIterations).toDouble/1000.toDouble}s " +
            s"runtime/clap_wrapper.sh $bin $executionIterations" !!(logger))
          val runtimes = getRuntimeFromClap(result)

          execution match {
            case Median =>
              executionIterations match {
                case 1 => runtimes.apply(0)
                case _ => runtimes.sorted.apply(executionIterations/2)
              }
            case Minimum => runtimes.min
          }
        }
        case false => runtimes(0)
      }

      // update or init global best
      best = best match{
        case Some(value) => runtime.value < value match {
          case true => Some(runtime.value)
          case false => best
        }
        case None => Some(runtime.value)
      }

      val executionTime = TimeSpan.inMilliseconds(
        (System.currentTimeMillis() - executionStart).toDouble
      )

      (
        Right(runtime),
        Some(compilationTime),
        Some(executionTime)
      )
    } catch {
      // todo check error codes here
      case e: Throwable => {
        val executionTime = TimeSpan.inMilliseconds(
          (System.currentTimeMillis() - executionStart).toDouble
        )
        (
          Left(AutoTuningError(
            EXECUTION_ERROR,
            Some(e.toString)
          )),
          Some(compilationTime),
          Some(executionTime)
        )
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
