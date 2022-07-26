package exploration.runner

import elevate.core.Strategy
import elevate.heuristic_search.{ExplorationResult, Runner}
import elevate.heuristic_search.util.{Solution, hashProgram}
import exploration.runner
import rise.elevate.Rise
import shine.C.AST._
import util.{Time, TimeSpan, gen}

import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.sys.process._

case class DebugExecutor(lowering: Strategy[Rise],
                         goldExpression: Rise,
                         iterations: Int,
                         inputSize: Int,
                         threshold: Double,
                         output: String
                        ) extends Runner[Rise] {


  case class TuningResultStatistic(
                                    number: Int,
                                    solution: String,
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

  val tuningResults = new ListBuffer[TuningResultStatistic]()
  var number = 0
  val random = new scala.util.Random

  // todo make this generic
  def checkSolution(solution: Solution[Rise]): Boolean = {
    //    runner.checkSolution(lowering, solution)
    runner.checkSolutionC(lowering, solution)
  }

  def execute(solution: Solution[Rise]): ExplorationResult[Rise] = {
    number = number + 1

    // throw the dices

    // save statistics
    val loweringDuration = 0
    val tuningDuration = 0
    val totalDuration = 1

    val samples = 1
    val executions = 1


    //    val result = randomFunction(number)
    //    val result = decrease(number)
    //    val result = increase(number)
    //    val result = constant()
    val result = performanceModel(solution)

    //    println("result: " + result)

    // convert Double to timespan for tuning output
    val resultTuning: Option[TimeSpan[Time.ms]] = result match {
      case Some(value) => Some(TimeSpan.inMilliseconds(value))
      case None => None
    }


    val tresult =
      TuningResultStatistic(
        number = number,
        solution = hashProgram(solution.expression),
        timestamp = System.currentTimeMillis(),
        duration = TimeSpan.inMilliseconds(totalDuration.toDouble),
        durationTuning = TimeSpan.inMilliseconds(tuningDuration.toDouble),
        durationLowering = TimeSpan.inMilliseconds(loweringDuration.toDouble),
        samples = samples,
        executions = executions,
        runtime = resultTuning
      )

    saveTuningResults(tresult)
    // add to
    tuningResults += tresult

    //    core.freshName.reset()

    ExplorationResult(solution, result, Option.empty)
  }

  def randomFunction(number: Int): Option[Double] = {
    if (number == 1) {
      // make sure initialization always succeeds

      Some(random.nextInt(100).toDouble + 1.0)
    } else {
      // get random performance value
      val index = random.nextInt(100)

      // chance to fail 50 %
      val runtime = index < 50 match {
        case true => Some(index.toDouble)
        case false => None
      }

      runtime
    }
  }

  def decrease(number: Int): Option[Double] = {
    Some(100.0 / (number * 0.9))
  }

  def increase(number: Int): Option[Double] = {
    Some(number.toDouble)
  }

  def constant(): Option[Double] = {
    Some(1.0)
  }

  var counter = 0

  def performanceModel(solution: Solution[Rise]): Option[Double] = {
    // evaluate, if expression is invalid
    //    counter += 1

    //    if(counter <= 10){
    // try to maximize programs size
    val value = 100000 / solution.expression.toString.size.toDouble

    //          val value = solution.expression.toString.size

    //      Some(value)
    //    }else{
    //      val value = solution.expression.toString.size
    //
    Some(value)
    //    }
  }

  def performanceModel2(solution: Solution[Rise]): Option[Double] = {
    // WARNING: Only for C, not opencl, implementation is not generic

    println("solution: " + hashProgram(solution.expression))
    println("strategies: " + solution.strategies.mkString(", "))

    //    val code = gen.openmp.function("riseFun").asStringFromExpr(lowered.get)
    //    println("code: \n" + code)

    // generate code
    val lowered = lowering.apply(solution.expression)
    val p = gen.openmp.function("riseFun").fromExpr(lowered.get)

    // todo check case of multiple functions
    val function = p.functions(0)

    val result = price(function.code.body)

    println("result: " + result)

    Some(result)
  }

  def priceExpr(expr: shine.C.AST.Expr): Double = {
    // todo implement this

    1.0
  }

  // determine number of iterations
  def countForLoop(stmt: Stmt, expr: shine.C.AST.Expr, expr1: shine.C.AST.Expr): Double = {

    // todo check initial value
    // initial value (mostly 0)

    // todo check assignment

    val bound = expr match {
      case BinaryExpr(one, two, three) => three match {
        case Literal(str) => str.toInt
        case _ => 0
      }
      case _ => 0
    }

    //    println("bound: " + bound)

    //    512
    bound
  }

  def price(stmt: Stmt): Double = {
    //    println()

    stmt match {

      // just call
      case Block(value) => // recursive for each statement of block
        //        println("block: " + value)
        value.map(elem => price(elem)).reduceLeft(_ + _)

      case Stmts(stmt, stmt1) => // recursive for each elem
        //        println("Stmts: " + stmt + " - " + stmt1)
        price(stmt) + price(stmt1)

      // count iterations
      case ForLoop(stmt, expr, expr1, stmt1) => // count iterations, then recursive on body
        //        println("for loop: " + stmt + " - " + expr + " - " + expr1 + " - " + stmt1)
        price(stmt) + countForLoop(stmt, expr, expr1) * (priceExpr(expr) + priceExpr(expr1) + price(stmt1))

      // count
      case DeclStmt(decl) =>
        //        println("declStmt: " + decl)
        1.0 // count
      case ExprStmt(expr) =>
        //        println("exprStmt: " + expr)
        1.0 // count

      case _ => 0.0
      //
      //      // ignore
      //      case Comment(str) => // ignore
      //      // not sure for now
      //      case IfThenElse(expr, stmt, maybeStmt) => // ignore for now
      //      case break: Break => // ignore for now
      //      case Code(str) => // ignore for now
      //      case value: Return => // ignore for now
      //      case GOTO(str) => // ignore for now
      //      case continue: Continue => // ignore for now
      //      case SynchronizeWarp() => // ignore for now
      //      case SynchronizeThreads() => // ignore for now
      //      case WhileLoop(expr, stmt) => // ignore for now
      //      case Barrier(local, global) => // ignore for now
    }
  }

  def plot() = {

    // also write config file

    val doe = tuningResults.size

    val configString = {
      s"""{
      "application_name": "mv_exploration",
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
    val fWriter = new PrintWriter(new FileOutputStream(new File(configFilePath), true))
    fWriter.write(configString)
    fWriter.close()

    // plot results
    //    val outputFilePath = output + "/" + "tuningStatistics_hm.csv"

    // mkdir output folder
    (s"mkdir -p ${output}/hm " !!)
    (s"mv ${output}/tuningStatistics_hm.csv ${output}/hm" !!)

    // call plot
    (s"hm-plot-optimization-results -j ${configFilePath} -i ${output}/hm -l exploration -o ${output}/plot.pdf --y_label 'Log Runtime(ms)' --title exploration" !!)

  }


  // todo check, if necessary
  def saveTuningResults(tuningResultStatistic: TuningResultStatistic) = {

    val filePath = output + "/" + "tuningStatistics.csv"
    val filePathHm = output + "/" + "tuningStatistics_hm.csv"

    val exists = Files.exists(Paths.get(filePath))

    val fWriter = new PrintWriter(new FileOutputStream(new File(filePath), true))
    val fWriterHm = new PrintWriter(new FileOutputStream(new File(filePathHm), true))

    if (!exists) {
      val header = "number, solution, timestamp, duration, durationTuning, durationLowering, samples, executions, runtime" + "\n"
      val headerHm = "index,runtime,Valid,Timestamp" + "\n"

      fWriter.write(header)
      fWriterHm.write(headerHm)
    }

    val (runtime, valid) = tuningResultStatistic.runtime match {
      case Some(value) => (value.value.toString, "True")
      case None => ("-1", "False")
    }

    // write line
    val line =
      tuningResultStatistic.number.toString + ", " +
        tuningResultStatistic.solution + ", " +
        tuningResultStatistic.timestamp.toString + ", " +
        tuningResultStatistic.duration.value.toString + ", " +
        tuningResultStatistic.durationTuning.value.toString + ", " +
        tuningResultStatistic.durationLowering.value.toString + ", " +
        tuningResultStatistic.samples.toString + ", " +
        tuningResultStatistic.executions.toString + ", " +
        runtime.toString + "\n"

    // write line hm
    val lineHm =
      tuningResultStatistic.number.toString + "," +
        runtime.toString + "," +
        valid + "," +
        tuningResultStatistic.timestamp.toString + "\n"

    fWriter.write(line)
    fWriterHm.write(lineHm)

    fWriter.close()
    fWriterHm.close()


  }
}
