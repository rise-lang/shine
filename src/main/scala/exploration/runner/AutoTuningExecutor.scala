package exploration.runner

import arithexpr.arithmetic.RangeMul
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.heuristic_search.Runner
import elevate.heuristic_search.util.{RewriteIdentifier, Solution, SolutionStep, hashProgram, hashSolution}
import rise.autotune.{AutoTuningError, AutoTuningErrorLevel, EXECUTION_ERROR, HostCode, Median, Timeouts, Tuner, getBest, getDuration, getSamples, search, tuningParam, wrapOclRun}
import rise.core.Expr
import rise.core.types.Nat
import rise.elevate.Rise
import rise.eqsat.Rewrite
import shine.OpenCL.{GlobalSize, LocalSize}
import util.{Time, TimeSpan}
import elevate.heuristic_search._
import exploration.{C_Backend, ExecutionBackend, OpenCL_Backend}
import exploration.explorationUtil.ExplorationErrorLevel.ExplorationErrorLevel

import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.sys.process._

case class AutoTuningExecutor(lowering: Strategy[Rise],
                              goldExpression: Rise,
                              hostCode: Option[HostCode],
                              iterations: Int,
                              inputSize: Int,
                              threshold: Double,
                              output: String,
                              samples: Int = 5,
                              executionBackend: ExecutionBackend = OpenCL_Backend
                             ) extends Runner[Rise] {

  case class TuningResultStatistic(
                                    number: Int,
                                    solution: Solution[Rise],
                                    timestamp: Long,
                                    duration: TimeSpan[Time.ms],
                                    durationTuning: TimeSpan[Time.ms],
                                    durationLowering: TimeSpan[Time.ms],
                                    samples: Int,
                                    executions: Int,
                                    runtime: Option[TimeSpan[Time.ms]]
                                  )

  case class Statistics(
                         samples: ListBuffer[TuningResultStatistic]
                       )

  case class ExecutionStatistics(
                                  performanceValue: Double,
                                  min: Double,
                                  max: Double,
                                  std: Double
                                ) {
    override def toString() = {
      s"""
         |PerformanceValue: ${performanceValue}
         |Min: ${min}
         |Max: ${max}
         |std: ${std}
         |""".stripMargin
    }
  }


  val tuningResults = new ListBuffer[TuningResultStatistic]()
  var number = 0
  val random = new scala.util.Random

  var counterTotal = 0
  var counterExpressions = 0

  var duration: Long = 0

  writeHeader(output + "/" + "executor.csv")

  // todo adjust header

  def plot(): Unit = {

    // also write config file
    val doe = number

    val configString = {
      s"""{
      "application_name": "mm_exploration",
      "optimization_objectives": ["runtime"],
      "feasible_output" : {
        "enable_feasible_predictor" : true,
        "name" : "Valid",
        "true_value" : "True",
        "false_value" : "False"
      },
      "hypermapper_mode" : {
        "mode" : "client-server"
      },
      "design_of_experiment": {
        "doe_type": "random sampling",
        "number_of_samples": ${doe}
      },
      "optimization_iterations": 0,
      "input_parameters" : {
        "index": {
        "parameter_type" : "integer",
        "values" : [0, ${doe}],
        "dependencies" : [],
        "constraints" : []
      }
      }
    }"""
    }

    // write configstring
    val configFilePath = output + "/" + "tuningStatistics.json"
    val fWriter = new PrintWriter(new FileOutputStream(new File(configFilePath), false))
    fWriter.write(configString)
    fWriter.close()

    // plot results
    //    val outputFilePath = output + "/" + "tuningStatistics_hm.csv"

    output + "/" + "executor.csv"

    // mkdir output folder
    (s"mkdir -p ${output}/hm " !!)
    (s"cp ${output}/executor_hm.csv ${output}/hm" !!)

    //    val expertConfig = expert match {
    //      case Some(value) => s"--exp ${value}"
    //      case None => ""
    //    }

    // performance evolution plot
    try {
      // call plot
      val command = s"python3 ${exploration.configuration.tunerRoot}/hypermapper/plot_optimization_results.py -j ${configFilePath} -i ${output}/hm -l exploration -o ${output}/plot.pdf --y_label 'Log Runtime(ms)' --title exploration "
      val command2 = s"python3 ${exploration.configuration.tunerRoot}/hypermapper/plot_optimization_results.py -j ${configFilePath} -i ${output}/hm -l exploration -o ${output}/plot_log.pdf --plot_log --y_label 'Log Runtime(ms)' --title exploration "
      println("plot: " + command)
      (command !!)
      println("plotlog: " + command2)
      (command2 !!)
    } catch {
      case e: Throwable => // ignore
    }


    // scatter plot
    try {
      val command = s"python exploration/plotting/plot.py --plot scatter --src ${output}/hm --title exploration --output ${output}/scatter.pdf"
      val command_log = s"python exploration/plotting/plot.py --plot scatter --src ${output}/hm --title exploration --output ${output}/scatter_log.pdf --log"

      println("scatter: " + command)
      (command !!)
      println("scatter log: " + command_log)
      (command_log !!)

    } catch {
      case e: Throwable => // ignore
    }

  }

  override def checkSolution(solution: Solution[Rise]): Boolean = {
    //    val checkStart = System.currentTimeMillis()
    val result = exploration.runner.checkSolution(lowering, solution)
    //    val result = exploration.runner.checkSolutionC(lowering)(solution)
    //    duration += System.currentTimeMillis() - checkStart
    //    println("checking duration total: " + duration.toDouble/1000 + " s")
    //    println("checking duration total: " + duration.toDouble/1000/60 + " m")

    result
  }

  // todo check output
  // high-level
  // low-level hash
  def executeC(solution: Solution[Rise]): ExplorationResult[Rise] = {

    // define and prepare executor executor
    val executor = CExecutor(
      lowering = lowering,
      goldExpression = goldExpression,
      iterations = 5, // check this
      inputSize = 128, // check this
      threshold = 100,
      output = output,
      saveToDisk = false
    )

    // define execution function
    val executeInternal: Expr => (
      Either[AutoTuningError, Double],
        Option[Double],
        Option[Double],
        Option[Double]
      ) = s => {

      val executionStart = System.currentTimeMillis()

      val result = executor.execute(s)

      // todo move to other thing
      val runtime: Either[AutoTuningError, Double] = result match {
        case Some(value) => Right(value)
        case None => Left(AutoTuningError(EXECUTION_ERROR, None))
      }

      // todo measure these properly
      val codegenTime = (System.currentTimeMillis() - executionStart).toDouble
      val compilationTime = (System.currentTimeMillis() - executionStart).toDouble
      val executionTime = (System.currentTimeMillis() - executionStart).toDouble

      (runtime,
        Some(codegenTime),
        Some(compilationTime),
        Some(executionTime))
    }

    val totalDurationStart = System.currentTimeMillis()

    number = number + 1

    // each call of execute save following information
    // +1 solution
    // number of samples
    // number of executions
    // duration

    // todo work with gold expression

    println("solution: " + solution)
    println(hashProgram(solution.expression()))

    // create tuner
    val tuner = Tuner(
      hostCode = HostCode("", "", ""), // we don't need that
      samples = iterations,
      name = "mm",
      output = "exploration/",
      timeouts = Timeouts(100000, 100000, 100000), // we might want to adjust this
      executionIterations = 10,
      speedupFactor = threshold,
      runtimeStatistic = Median,
      configFile = None,
      //      Some("/home/jo/development/rise-lang/shine/autotuning/scal/scal.json"),
      hmConstraints = true,
      executor = Some(executeInternal),
      //      hmConstraints = false,
      saveToFile = false
    )

    // lower expression
    val loweringDurationStart = System.currentTimeMillis()
    val lowered = lowering.apply(solution.expression())
    val loweringDuration = System.currentTimeMillis() - loweringDurationStart

    val (result, statistic) = lowered match {
      case Success(p) => {

        // run tuning
        val tuningDurationStart = System.currentTimeMillis()
        val (runtime, tuningStatistic) = try {
          val result = search(tuner)(lowered.get)

          // meta information
          //          val duration = getDuration(result)
          val samples = getSamples(result)

          result.samples.foreach(println)

          //          println("samples: " + samples)

          val best = getBest(result.samples)
          //          println("best: " + best)
          //          println("lowered: " + lowered)

          val runtime = best match {
            case Some(_) =>
              best.get.runtime match {
                case Right(value) => Some(TimeSpan.inMilliseconds(value.value))
                case Left(value) => None
              }
            case None => None
          }

          val tuningDuration = System.currentTimeMillis() - tuningDurationStart
          val totalDuration = System.currentTimeMillis() - totalDurationStart

          (
            runtime,
            TuningResultStatistic(
              number = number,
              solution = solution,
              timestamp = System.currentTimeMillis(),
              duration = TimeSpan.inMilliseconds(totalDuration.toDouble),
              durationTuning = TimeSpan.inMilliseconds(tuningDuration.toDouble),
              durationLowering = TimeSpan.inMilliseconds(loweringDuration.toDouble),
              samples = samples,
              executions = tuner.executionIterations * samples,
              runtime
            )
          )
        } catch {
          case e: Throwable =>

            println("tuning is brorken! mey friend")
            println("e: " + e)


            val tuningDuration = System.currentTimeMillis() - tuningDurationStart
            val totalDuration = System.currentTimeMillis() - totalDurationStart

            (
              None,
              TuningResultStatistic(
                number = number,
                solution = solution,
                timestamp = System.currentTimeMillis(),
                duration = TimeSpan.inMilliseconds(totalDuration.toDouble),
                durationTuning = TimeSpan.inMilliseconds(tuningDuration.toDouble),
                durationLowering = TimeSpan.inMilliseconds(loweringDuration.toDouble),
                samples = 0,
                executions = 0,
                None
              )
            )
        }

        (
          (solution.expression(), runtime),
          tuningStatistic
        )
      }
      case Failure(s) =>

        // duration lowering
        // measure

        // durationTuning = 0
        val totalDuration = System.currentTimeMillis() - totalDurationStart

        (
          (solution.expression(), None),
          TuningResultStatistic(
            number = number,
            solution = solution,
            timestamp = System.currentTimeMillis(),
            duration = TimeSpan.inMilliseconds(totalDuration.toDouble),
            durationTuning = TimeSpan.inMilliseconds(0.0),
            durationLowering = TimeSpan.inMilliseconds(loweringDuration.toDouble),
            samples = 0,
            executions = 0,
            None
          )
        )
    }

    saveTuningResults(statistic, solution, lowered.get)

    // convert from Option[TimeSpan] to Double
    val resultingRuntime = result._2 match {
      case Some(value) => Some(value.value)
      case None => None
    }

    (result._1, resultingRuntime)
    ExplorationResult(
      solution,
      resultingRuntime,
      None
    )
  }


  // wrapper for different backends
  def execute(solution: Solution[Rise]): ExplorationResult[Rise] = {
    executionBackend match {
      case C_Backend => executeC(solution)
      case OpenCL_Backend => executeOpenCL(solution)
    }
  }

  def executeOpenCL(solution: Solution[Rise]): ExplorationResult[Rise] = {
    val totalDurationStart = System.currentTimeMillis()

    number = number + 1

    // each call of execute save following information
    // +1 solution
    // number of samples
    // number of executions
    // duration

    // todo work with gold expression

    println("solution: " + solution)
    println(hashProgram(solution.expression()))

    // todo adjust this for autotuning benchmarks
    val tuner = Tuner(
      hostCode = hostCode.get, // we don't need that, yes we do!
      samples = samples,
      name = "mm",
      output = "exploration",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      //      configFile = Some("autotuning/config/mmCPU/rs_cot_1024.json"),
      configFile = None,
      hmConstraints = true,
      executor = None,
      saveToFile = true,
      tunerRoot = exploration.configuration.tunerRoot
    )

    // lower expression
    val loweringDurationStart = System.currentTimeMillis()
    val lowered = lowering.apply(solution.expression())
    val loweringDuration = System.currentTimeMillis() - loweringDurationStart

    val (result, statistic) = lowered match {
      case Success(p) => {

        // now wrap ocl
        val eTuning: Expr =
          tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
            tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
              tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
                tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
                  wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered.get)
                ))))

        // run tuning
        val tuningDurationStart = System.currentTimeMillis()
        val (runtime, tuningStatistic) = try {
          val result = search(tuner)(eTuning)

          // meta information
          //          val duration = getDuration(result)
          val samples = getSamples(result)

          result.samples.foreach(println)

          //          println("samples: " + samples)

          // write samples (append) to executor output file
          //          counterTotal += result.samples.size
          counterExpressions += result.samples.size // why?

          result.samples.foreach(sample => {

            val (performanceValue, errorLevel): (Option[Double], Option[AutoTuningErrorLevel]) = sample.runtime match {
              case Left(error) => (None, Some(error.errorLevel))
              case Right(runtime) => (Some(runtime.value), None)
            }

            writeValues(
              path = output + "/" + "executor.csv",
              result = (solution, lowered.get, performanceValue, errorLevel),
              statistics = None,
              solution.rewrites(),
              "executor"
            )
          })

          val best = getBest(result.samples)
          //          println("best: " + best)
          //          println("lowered: " + lowered)

          val runtime = best match {
            case Some(_) =>
              best.get.runtime match {
                case Right(value) => Some(TimeSpan.inMilliseconds(value.value))
                case Left(value) => None
              }
            case None => None
          }

          val tuningDuration = System.currentTimeMillis() - tuningDurationStart
          val totalDuration = System.currentTimeMillis() - totalDurationStart

          (
            runtime,
            TuningResultStatistic(
              number = number,
              solution = solution,
              timestamp = System.currentTimeMillis(),
              duration = TimeSpan.inMilliseconds(totalDuration.toDouble),
              durationTuning = TimeSpan.inMilliseconds(tuningDuration.toDouble),
              durationLowering = TimeSpan.inMilliseconds(loweringDuration.toDouble),
              samples = samples,
              executions = tuner.executionIterations * samples,
              runtime
            )
          )
        } catch {
          case e: Throwable =>

            println("tuning is brorken! mey friend")
            println("e: " + e)


            val tuningDuration = System.currentTimeMillis() - tuningDurationStart
            val totalDuration = System.currentTimeMillis() - totalDurationStart

            (
              None,
              TuningResultStatistic(
                number = number,
                solution = solution,
                timestamp = System.currentTimeMillis(),
                duration = TimeSpan.inMilliseconds(totalDuration.toDouble),
                durationTuning = TimeSpan.inMilliseconds(tuningDuration.toDouble),
                durationLowering = TimeSpan.inMilliseconds(loweringDuration.toDouble),
                samples = 0,
                executions = 0,
                None
              )
            )
        }

        (
          (solution.expression(), runtime),
          tuningStatistic
        )
      }
      case Failure(s) =>

        // duration lowering
        // measure

        // durationTuning = 0
        val totalDuration = System.currentTimeMillis() - totalDurationStart

        (
          (solution.expression(), None),
          TuningResultStatistic(
            number = number,
            solution = solution,
            timestamp = System.currentTimeMillis(),
            duration = TimeSpan.inMilliseconds(totalDuration.toDouble),
            durationTuning = TimeSpan.inMilliseconds(0.0),
            durationLowering = TimeSpan.inMilliseconds(loweringDuration.toDouble),
            samples = 0,
            executions = 0,
            None
          )
        )
    }

    saveTuningResults(statistic, solution, lowered.get)

    // convert from Option[TimeSpan] to Double
    val resultingRuntime = result._2 match {
      case Some(value) => Some(value.value)
      case None => None
    }

    (result._1, resultingRuntime)
    ExplorationResult(
      solution,
      resultingRuntime,
      None
    )
  }

  def saveTuningResults(
                         tuningResultStatistic: TuningResultStatistic,
                         solution: Solution[Rise],
                         lowered: Rise
                       ) = {

    val filePath = output + "/" + "tuning_executor.csv"
    val exists = Files.exists(Paths.get(filePath))
    val existsHM = Files.exists(Paths.get(filePath.substring(0, filePath.size - 4) + "_hm.csv"))

    val fWriter = new PrintWriter(new FileOutputStream(new File(filePath), true))
    val fHMWriter = new PrintWriter(new FileOutputStream(new File(filePath.substring(0, filePath.size - 4) + "_hm.csv"), true))

    // todo add complete parameter list if existing

    if (!exists) {
      //      val header = "number, solution, strategy, timestamp, duration, durationTuning, durationLowering, samples, executions, runtime, runtime2" + "\n"

      val header = "iteration,runner,timestamp,high-level hash,low-level hash,rewrite,error-level,runtime,min,max,std,executions,samples,duration,durationTuning,durationLowering\n"

      //      val string = "iteration,runner,timestamp,high-level hash,low-level hash,rewrite,error-level,runtime,min,max,std,executions \n"
      fWriter.write(header)
    }

    if (!existsHM) {
      val header = "index,runtime,Valid,Timestamp\n"
      fHMWriter.write(header)
    }

    val runtime = tuningResultStatistic.runtime match {
      case Some(value) => value.value.toString
      case None => "-1"
    }

    val runtime2 = tuningResultStatistic.runtime match {
      case Some(value) => value.value
      case None => "-1"
    }

    // write line
    val line =
      tuningResultStatistic.number.toString + "," + // iteration
        "executor" + "," + // runner
        tuningResultStatistic.timestamp.toString + "," + // timestamp
        hashSolution(solution) + "," + // high-level hash
        hashProgram(lowered) + "," + // low-level hash
        solution.rewrites().mkString("\"[", ",", "]\"") + "," + // rewrite
        "no error reports for tuning" + "," + // error level
        runtime + "," + // runtime
        runtime + "," + // min // best tuning sample -> should match runtime as runtime is defined as best tuning sample
        runtime + "," + // max // todo write worst tuning sample here
        "0" + "," + // std
        tuningResultStatistic.executions.toString + "," + // executions
        tuningResultStatistic.samples.toString + "," + // samples
        tuningResultStatistic.duration.toString + "," + // duration
        tuningResultStatistic.durationTuning.toString + "," + // durationTuning
        tuningResultStatistic.durationLowering.toString + "," + // durationLowering
        "\n"

    // write line
    val lineHM = tuningResultStatistic.runtime match {
      case Some(value) =>
        tuningResultStatistic.number.toString + "," +
          value.value.toString + "," +
          "True" + "," +
          System.currentTimeMillis().toString + "\n"
      case None =>
        tuningResultStatistic.number.toString + "," +
          "-1" + "," +
          "False" + "," +
          System.currentTimeMillis().toString + "\n"
    }

    fWriter.write(line)
    fHMWriter.write(lineHM)

    fWriter.close()
    fHMWriter.close()

    // copy files around to match metaheuristics requirements
    (s"mkdir -p ${output}/hm" !!)
    (s"cp -r ${output}/tuning_executor_hm.csv ${output}/hm" !!)

  }


  def writeValues(path: String,
                  result: (Solution[Rise], Rise, Option[Double], Option[AutoTuningErrorLevel]),
                  statistics: Option[ExecutionStatistics],
                  rewrite: Seq[RewriteIdentifier[Rise]],
                  name: String): Unit = {

    //    samples += 1

    // open file to append values
    val file = new PrintWriter(
      new FileOutputStream(new File(path), true))

    val fileHM = new PrintWriter(
      new FileOutputStream(new File(path.substring(0, path.size - 4) + "_hm.csv"), true))

    // create string to write to file
    var string = s"$counterTotal,$name,${System.currentTimeMillis().toString}," +
      hashSolution(result._1) + "," +
      hashProgram(result._2) + "," +
      rewrite.mkString("\"[", ",", "]\"") + "," +
      result._4.toString + ","

    result._3 match {
      case Some(value) => string += value.toString + ","
      case _ => string += "-1,"
    }

    statistics match {
      case Some(stats) => // print statistics
        string += s"${stats.min},${stats.max},${stats.std},${iterations}\n"
      case None =>
        string += s"None,None,None,${iterations}\n"
    }


    val stringHmAppendix = result._3 match {
      case Some(value) => value.toString + "," + "True" + "," + System.currentTimeMillis().toString + "\n"
      case None => "-1" + "," + "False" + "," + System.currentTimeMillis().toString + "\n"
    }

    val stringHm = s"${counterTotal}" + "," + stringHmAppendix

    // write to file and close
    file.write(string)
    fileHM.write(stringHm)

    counterTotal += 1

    // plot every 10 executions
    //    counter % printEvery match {
    //      case 0 => plot()
    //      case _ =>
    //    }


    file.close()
    fileHM.close()
  }

  def writeHeader(path: String): Unit = {
    // open file
    val file = new PrintWriter(
      new FileOutputStream(new File(path), false))

    val fileHM = new PrintWriter(
      new FileOutputStream(new File(path.substring(0, path.size - 4) + "_hm.csv"), false))

    //    println("hello: " + path.substring(0, path.size - 4) + "_hm.csv")

    // create string to write to file
    val string = "iteration,runner,timestamp,high-level hash," +
      "low-level hash,rewrite,error-level,runtime,min,max,std,executions \n"

    val stringHM = "index,runtime,Valid,Timestamp" + "\n"

    // write to file and close
    file.write(string)
    fileHM.write(stringHM)

    fileHM.close()
    file.close()
  }


}
