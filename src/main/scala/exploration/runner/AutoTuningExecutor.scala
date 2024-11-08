package exploration.runner

import arithexpr.arithmetic.RangeMul
import arithexpr.arithmetic.RangeAdd
import arithexpr.arithmetic.Range

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
import exploration.{C_Backend, ExecutionBackend, OpenCL_Backend, explore, tunerConfiguration}
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
                              inputSizes: Seq[Nat],
                              threshold: Double,
                              output: String,
                              samples: Int = 5,
                              global_size_limit: Int = 1024,
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


  trait OpenCLWrapping

  case object sequential_1 extends OpenCLWrapping

  case object parallel_10 extends OpenCLWrapping

  case object parallel_01 extends OpenCLWrapping

  case object parallel_11 extends OpenCLWrapping


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
      "${exploration.tunerConfiguration.tunerVersion}_mode" : {
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
      val command = s"${exploration.tunerConfiguration.pythonVersion} ${exploration.tunerConfiguration.tunerRoot}/${exploration.tunerConfiguration.plotter} -j ${configFilePath} -i ${output}/hm -l exploration -o ${output}/plot.pdf --y_label 'Log Runtime(ms)' --title exploration "
      val command2 = s"${exploration.tunerConfiguration.pythonVersion} ${exploration.tunerConfiguration.tunerRoot}/${exploration.tunerConfiguration.plotter} -j ${configFilePath} -i ${output}/hm -l exploration -o ${output}/plot_log.pdf --plot_log --y_label 'Log Runtime(ms)' --title exploration "
      println("plot: " + command)
      (command !!)
      println("plotlog: " + command2)
      (command2 !!)
    } catch {
      case e: Throwable => // ignore
        println("ignore for now")
    }


    // scatter plot
    try {
      val command = s"${exploration.tunerConfiguration.pythonVersion} exploration/plotting/plot.py --plot scatter --src ${output}/hm --title exploration --output ${output}/scatter.pdf"
      val command_log = s"${exploration.tunerConfiguration.pythonVersion} exploration/plotting/plot.py --plot scatter --src ${output}/hm --title exploration --output ${output}/scatter_log.pdf --log"

      println("scatter: " + command)
      (command !!)
      println("scatter log: " + command_log)
      (command_log !!)

    } catch {
      case e: Throwable => // ignore
        println("ignore for now")
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
      iterations = iterations, // check this
      inputSize = 128, // check this
      threshold = 100,
      output = output,
      saveToDisk = false
    )

    // define execution function
    val executeInternal: Expr => rise.autotune.ExecutionResult = s => {

      val executionStart = System.currentTimeMillis()

      val result = executor.executeTuning(s)

      result
    }

    val totalDurationStart = System.currentTimeMillis()

    number = number + 1

    // each call of execute save following information
    // +1 solution
    // number of samples
    // number of executions
    // duration

    // todo work with gold expression

    //    println("solution: " + solution)
    println("solution: " + hashProgram(solution.expression()))

    // create tuner
    val tuner = Tuner(
      hostCode = HostCode("", "", ""), // we don't need that here
      executor = Some(executeInternal), // we use this as executor
      samples = samples,
      name = "mm",
      output = "exploration",
      timeouts = Timeouts(
        codegenerationTimeout = 100000,
        compilationTimeout = 100000,
        executionTimeout = 100000
      ),
      executionIterations = iterations,
      speedupFactor = threshold,
      runtimeStatistic = Median,
      configFile = None,
      hmConstraints = true,
      disableChecking = true,
      saveToFile = true,
      tunerRoot = exploration.tunerConfiguration.tunerRoot,
      tunerPath = exploration.tunerConfiguration.tuner,
      tunerPlot = exploration.tunerConfiguration.plotter,
      tunerPython = exploration.tunerConfiguration.pythonVersion,
      tunerVersion = exploration.tunerConfiguration.tunerVersion,
      tunerTimeBudgetCot = exploration.tunerConfiguration.tunerTimeBudgetCot
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
          println("tuning finished here")
          val samples = getSamples(result)

          println("print samples")
          result.samples.foreach(println)

          //          println("samples: " + samples)
          counterExpressions += result.samples.size // why?

          println("write values ")
          result.samples.foreach(sample => {

            val (performanceValue, error): (Option[Double], Option[AutoTuningError]) = sample.runtime match {
              case Left(error) => (None, Some(error))
              case Right(runtime) => (Some(runtime.value), None)
            }

            writeValues(
              path = output + "/" + "executor.csv",
              result = (solution, lowered.get, sample.parameters, performanceValue, error, sample.statistics),
              statistics = None,
              solution.rewrites(),
              "executor"
            )
          })

          println("get best")
          val best = getBest(result.samples)
          println("best: " + best)
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
              runtime,
              //              None,
              //              None,
              //              None,
            )
          )
        } catch {
          case e: Throwable =>
            println("tuning finished with error: " + e)

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
                None,
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
            None,
            //            None,
            //            None,
            //            None
          )
        )
    }


    // save results
    lowered match {
      case Success(s) =>
        saveTuningResults(
          tuningResultStatistic = statistic,
          solution = solution,
          lowered = Some(s)
        )
      case Failure(s) =>
        saveTuningResults(
          tuningResultStatistic = statistic,
          solution = solution,
          lowered = None
        )
    }

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

  private def countMapGlobal(dimension: Int)(solution: Solution[Rise]): Int = {
    solution.solutionSteps.count(step =>
      step.strategy.equals(rise.elevate.rules.lowering.mapGlobal(dimension))
    )
  }

  private def countMapWorkGroup(dimension: Int)(solution: Solution[Rise]): Int = {
    solution.solutionSteps.count(step =>
      step.strategy.equals(rise.elevate.rules.lowering.mapWorkGroup(dimension))
    )
  }

  private def countMapLocal(dimension: Int)(solution: Solution[Rise]): Int = {
    solution.solutionSteps.count(step =>
      step.strategy.equals(rise.elevate.rules.lowering.mapLocal(dimension))
    )
  }


  private def getOclDimensions(solution: Solution[Rise]): OpenCLWrapping = {

    // count applications of parallel patterns
    val mg0 = countMapGlobal(0)(solution)
    val mg1 = countMapGlobal(1)(solution)

    val mwg0 = countMapWorkGroup(0)(solution)
    val mwg1 = countMapWorkGroup(1)(solution)

    val ml0 = countMapLocal(0)(solution)
    val ml1 = countMapLocal(1)(solution)

    mg0 + mwg0 + ml0 match {
      case 0 =>
        mg1 + mwg1 + ml1 match {
          case 0 => sequential_1
          case _ => parallel_01
        }
      case _ =>
        mg1 + mwg1 + ml1 match {
          case 0 => parallel_10
          case _ => parallel_11
        }
    }
  }

  private def wrapOCLDimensions(dimensions: OpenCLWrapping, e: Expr): Expr = {

    dimensions match {

      case _: sequential_1.type =>
        tuningParam("ls0", RangeMul(1, global_size_limit, 2), 1, (ls0: Nat) =>
          tuningParam("gs0", RangeMul(1, global_size_limit, 2), 1024, (gs0: Nat) =>
            wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(e)
          ))

      case _: parallel_10.type =>

        tuningParam("ls0", RangeMul(1, global_size_limit, 2), 32, (ls0: Nat) =>
          tuningParam("gs0", RangeMul(1, global_size_limit, 2), 1024, (gs0: Nat) =>
            wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(e)
          ))

      case _: parallel_01.type =>

        tuningParam("ls1", RangeMul(1, global_size_limit, 2), 32, (ls1: Nat) =>
          tuningParam("gs1", RangeMul(1, global_size_limit, 2), 1024, (gs1: Nat) =>
            wrapOclRun(LocalSize(1, ls1), GlobalSize(1, gs1))(e)
          ))

      case _: parallel_11.type =>

        tuningParam("ls0", RangeMul(1, global_size_limit, 2), 32, (ls0: Nat) =>
          tuningParam("ls1", RangeMul(1, global_size_limit, 2), 32, (ls1: Nat) =>
            tuningParam("gs0", RangeMul(1, global_size_limit, 2), 1024, (gs0: Nat) =>
              tuningParam("gs1", RangeMul(1, global_size_limit, 2), 1024, (gs1: Nat) =>
                wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(e)
              ))))
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

    //    println("solution: " + solution)
    println("solution: " + hashProgram(solution.expression()))

    //    check_constraints =

    // todo adjust this for autotuning benchmarks
    val tuner = Tuner(
      hostCode = hostCode.get, // we don't need that, yes we do!
      inputSizes = inputSizes, // pass through input sizes
      samples = samples,
      name = "mm", // adjust name here
      output = "exploration",
      timeouts = Timeouts(
        codegenerationTimeout = 30000,
        compilationTimeout = 30000,
        executionTimeout = 30000
      ),
      executionIterations = iterations,
      speedupFactor = threshold,
      //      configFile = Some("autotuning/config/mmCPU/rs_cot_1024.json"),
      configFile = None,
      hmConstraints = true,
      executor = None,
      saveToFile = true,
      tunerRoot = exploration.tunerConfiguration.tunerRoot,
      tunerTimeBudgetCot = exploration.tunerConfiguration.tunerTimeBudgetCot,
      disableChecking = true
    )

    // lower expression
    val loweringDurationStart = System.currentTimeMillis()
    val lowered = lowering.apply(solution.expression())
    val loweringDuration = System.currentTimeMillis() - loweringDurationStart

    //    println("lowered: \n" + lowered)

    val (result, statistic) = lowered match {
      case Success(e) => {

        // get case of parallelism to define default values for auto-tuning
        val oclDimensions: OpenCLWrapping = getOclDimensions(solution) // get from solution or lowered solution?
        val eTuning: Expr = wrapOCLDimensions(oclDimensions, e)

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

            val (performanceValue, error): (Option[Double], Option[AutoTuningError]) = sample.runtime match {
              //              case Left(error) => (None, Some(error.errorLevel))
              case Left(error) => (None, Some(error))
              case Right(runtime) => (Some(runtime.value), None)
            }

            writeValues(
              path = output + "/" + "executor.csv",
              result = (solution, lowered.get, sample.parameters, performanceValue, error, sample.statistics),
              statistics = None,
              solution.rewrites(),
              "executor"
            )
          })

          val best = getBest(result.samples)
          //          println("best: " + best)
          //          println("lowered: " + lowered)

          val min: Option[TimeSpan[Time.ms]] = result.samples.map(elem => elem.runtime match {
            case Right(value) => Some(value)
            case Left(error) => None
          }).reduceLeft((A, B) => {
            (A, B) match {
              case (Some(runtimeA), Some(runtimeB)) => {
                runtimeA.value <= runtimeB.value match {
                  case true => Some(runtimeA)
                  case false => Some(runtimeB)
                }
              }
              case (None, None) => None
              case (Some(runtimeA), None) => Some(runtimeA)
              case (None, Some(runtimeB)) => Some(runtimeB)
            }
          })

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
              runtime,

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

    // save results
    lowered match {
      case Success(s) =>
        saveTuningResults(
          tuningResultStatistic = statistic,
          solution = solution,
          lowered = Some(s)
        )
      case Failure(s) =>
        saveTuningResults(
          tuningResultStatistic = statistic,
          solution = solution,
          lowered = None
        )
    }

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
                         lowered: Option[Rise]
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

    val lowered_hash = lowered match {
      case Some(lowered_expression) => hashProgram(lowered_expression)
      case None => ""
    }

    // write line
    val line =
      tuningResultStatistic.number.toString + "," + // iteration
        "executor" + "," + // runner
        tuningResultStatistic.timestamp.toString + "," + // timestamp
        hashSolution(solution) + "," + // high-level hash
        lowered_hash + "," + // low-level hash
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
                  result: (Solution[Rise], Rise, Map[String, rise.autotune.TuningParameterValues], Option[Double], Option[AutoTuningError], rise.autotune.SampleStatistics),
                  statistics: Option[ExecutionStatistics],
                  rewrite: Seq[RewriteIdentifier[Rise]],
                  name: String): Unit = {

    //    samples += 1

    // open file to append values
    val file = new PrintWriter(
      new FileOutputStream(new File(path), true))

    val fileHM = new PrintWriter(
      new FileOutputStream(new File(path.substring(0, path.size - 4) + "_hm.csv"), true))

    val error_string: String = result._5 match {
      case None => "None,"
      case Some(error) =>
        error.message match {
          case Some(message) =>
            error.errorLevel.toString + "," + message.split("\n")(0)
          case None =>
            error.errorLevel.toString + ","
        }
    }

    // create string to write to file
    var string = s"$counterTotal,$name,${System.currentTimeMillis().toString}," +
      hashSolution(result._1) + "," +
      hashProgram(result._2) + "," +
      rewrite.filter(elem => elem.strategy != elevate.core.strategies.basic.id[Rise]).mkString("\"[", ",", "]\"") + "," +
      result._3.map(config => s"${config._1}=${config._2.value}").mkString("\"[", ",", "]\"") + "," + // parameter configuration here
      error_string + ","

    result._4 match {
      case Some(value) => string += value.toString + ","
      case _ => string += "-1,"
    }

    val minimumString = result._6.minimum match {
      case Some(value) => s"$value"
      case None => "None"
    }

    string += minimumString + ","

    val maximumString = result._6.maximum match {
      case Some(value) => s"$value"
      case None => "None"
    }

    string += maximumString + ","

    val stdString = result._6.standardDeviation match {
      case Some(value) => String.format("%.4f", value)
      case None => "None"
    }

    string += stdString + ","
    string += s"${result._6.iterations}\n"

    val stringHmAppendix = result._4 match {
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
      "low-level hash,rewrite,parameter-configuration,error-level,error,runtime,min,max,std,executions \n"

    val stringHM = "index,runtime,Valid,Timestamp" + "\n"

    // write to file and close
    file.write(string)
    fileHM.write(stringHM)

    fileHM.close()
    file.close()
  }


}
