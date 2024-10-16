package rise.autotune

import rise.core.Expr
import util.ExecuteOpenCL.{executorHeadersPath, includes, libDirs, libs, platformPath}
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
                           minimum: Option[TimeSpan[Time.ms]],
                           maximum: Option[TimeSpan[Time.ms]],
                           standardDeviation: Option[Double],
                           iterations: Int,
                          ) {

  override def toString: String = {

    val runtime_string: String = runtime match {
      case Right(value) => s"Runtime: ${value}"
      case Left(error) => s"${error.errorLevel}: ${error.message}"
    }

    s"""ExecutionResult:
      $runtime_string
      codegenTime: ${if (codegenTime.isDefined) codegenTime.get else "-"}
      compilationTime: ${if (compilationTime.isDefined) s"${compilationTime.get}" else "-"}
      executionTime: ${if (executionTime.isDefined) s"${executionTime.get}" else "-"}
      minimum: ${if (minimum.isDefined) s"${minimum.get}" else "-"}
      maximum: ${if (maximum.isDefined) s"${maximum.get}" else "-"}
      standardDeviation: ${if (standardDeviation.isDefined) s"${standardDeviation.get}" else "-"}
      iterations: $iterations
    """.stripMargin
  }
}


object execution {
  var best: Option[Double] = Some(5000)

  // logger to avoid printing of stderr
  val logger = new ProcessLogger {
    def out(s: => String): Unit = s

    def err(s: => String): Unit = s

    def buffer[T](f: => T): T = f
  }

  def executeC(expression: Expr,
               timeouts: Timeouts,
               executionIterations: Int,
               speedupFactor: Double,
               execution: RuntimeStatistic
              )
  : ExecutionResult = {
    // call C Executo here


    null
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
      val codgenResult: Option[gen.opencl.HostedModule] = autoTuningUtils.runWithTimeout(
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
      case e: Throwable =>
        Left(AutoTuningError(CODE_GENERATION_ERROR, Some(e.getCause.getMessage)))
    }

    val codegenTime = TimeSpan.inMilliseconds((System.currentTimeMillis() - codegenStart).toDouble)

    codegenResult match {
      case Right(generatedModule) => {

        val program =
          s"""
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

        //        println("program: \n" + program)

        //        System.exit(0)

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
          executionTime = result._3,
          minimum = result._4,
          maximum = result._5,
          standardDeviation = result._6,
          iterations = result._7
        )
      }
      case Left(error) =>
        ExecutionResult(
          runtime = Left(error),
          codegenTime = Some(codegenTime),
          compilationTime = None,
          executionTime = None,
          minimum = None,
          maximum = None,
          standardDeviation = None,
          iterations = 1
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
      Option[TimeSpan[Time.ms]], // execution time
      Option[TimeSpan[Time.ms]], // min
      Option[TimeSpan[Time.ms]], // max
      Option[Double], // std
      Int // iterations
    ) = {

    val src = writeToTempFile("code-", ".c", code).getAbsolutePath
    val bin = createTempFile("bin-", "").getAbsolutePath
    val sources = s"$src ${platformPath}buffer_$buffer_impl.c ${platformPath}ocl.c"

    val compilationStart = System.currentTimeMillis()
    try {
      (s"timeout ${compilationTimeout.toDouble / 1000.toDouble}s " +
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
    try {

      // execute once to check speedup factor
      val result = (s"timeout " +
        s"${(executionTimeout * 1).toDouble / 1000.toDouble}s " +
        s"runtime/clap_wrapper.sh $bin 1" !! (logger))
      val initialRun = getRuntimeFromClap(result)

      // check if speedup condition is met or current best is not initialized
      val repeat = best match {
        case Some(bestValue) => initialRun(0).value < bestValue * speedupFactor
        case None => true
      }

      // repeat execution with execution iterations
      val (runtime, minimum, maximum, std, iterations) = repeat match {
        case true => {

          // repeat execution with execution iterations
          val result = (s"timeout " +
            s"${(executionTimeout * (executionIterations - 1)).toDouble / 1000.toDouble}s " +
            s"runtime/clap_wrapper.sh $bin ${executionIterations - 1}" !! (logger))
          val runtimes = initialRun ++ getRuntimeFromClap(result)

          // min max value
          val minimum: Option[TimeSpan[Time.ms]] = Some(runtimes.sorted.apply(0))
          val maximum: Option[TimeSpan[Time.ms]] = Some(runtimes.sorted.last)

          // standard deviation
          val runtimesDoubles: Seq[Double] = runtimes.map(elem => elem.value)

          val std = runtimesDoubles.length <= 1 match {
            case true => None
            case false => {
              val mean = runtimesDoubles.sum / runtimesDoubles.length
              val variance = runtimesDoubles.map(x => math.pow(x - mean, 2)).sum / runtimesDoubles.length
              Some(scala.math.sqrt(variance))
            }
          }

          val resultingRuntime = execution match {
            case Median =>
              executionIterations match {
                case 1 => runtimes.apply(0)
                case _ => runtimes.sorted.apply(executionIterations / 2)
              }
            case Minimum => runtimes.min
          }

          (resultingRuntime, minimum, maximum, std, executionIterations)
        }

        case false => (initialRun(0), Some(initialRun(0)), Some(initialRun(0)), None, 1)
      }


      // update or init global best
      best = best match {
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
        Some(executionTime),
        minimum,
        maximum,
        std,
        iterations
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
          Some(executionTime),
          None,
          None,
          None,
          1
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
          .toDouble / 1000000)
    })

    runtimes
  }
}
