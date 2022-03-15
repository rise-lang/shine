package exploration.runner

import arithexpr.arithmetic.RangeMul
import elevate.core.Strategy
import elevate.heuristic_search.Runner
import elevate.heuristic_search.util.{Solution, hashProgram}
import exploration.runner
import rise.autotune
import rise.autotune.{tuningParam, wrapOclRun}
import rise.core.Expr
import rise.core.types.{Nat, NatIdentifier}
import rise.elevate.Rise
import shine.OpenCL.{GlobalSize, LocalSize}
import util.{Time, TimeSpan, gen}

import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer

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

//  override def checkSolution(solution: Solution[Rise]): Boolean = {
//
//    true
//  }

  def checkSolution(solution: Solution[Rise]): Boolean = {
    runner.checkSolution(lowering, solution)
  }

  def execute(solution: Solution[Rise]):(Rise, Option[Double]) = {
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

    println("result: " + result)

    // convert Double to timespan for tuning output
    val resultTuning: Option[TimeSpan[Time.ms]] = result match {
      case Some(value) => Some(TimeSpan.inMilliseconds(value))
      case None => None
    }


    saveTuningResults(
      TuningResultStatistic(
        number = number,
        solution = hashProgram(solution.expression),
        timestamp = System.currentTimeMillis(),
        duration = TimeSpan.inMilliseconds(totalDuration.toDouble),
        durationTuning = TimeSpan.inMilliseconds(tuningDuration.toDouble),
        durationLowering =  TimeSpan.inMilliseconds(loweringDuration.toDouble),
        samples = samples,
        executions = executions,
        runtime = resultTuning
      )
    )

      (solution.expression, result)
  }

  def randomFunction(number: Int): Option[Double] = {
    if (number == 1){
      // make sure initialization always succeeds

      Some(random.nextInt(100).toDouble + 1.0)
    }else{
      // get random performance value
      val index = random.nextInt(100)

      // chance to fail 50 %
      val runtime = index < 50 match{
        case true => Some(index.toDouble)
        case false => None
      }

      runtime
    }
  }

  def decrease(number: Int): Option[Double] = {
    Some(100.0/(number*0.9))
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
      val value = 100000/solution.expression.toString.size.toDouble

//          val value = solution.expression.toString.size

//      Some(value)
//    }else{
//      val value = solution.expression.toString.size
//
      Some(value)
//    }
  }


  // todo check, if necessary
  def saveTuningResults(tuningResultStatistic: TuningResultStatistic) = {

    val filePath = output + "/" + "tuningStatistics.csv"
    val exists = Files.exists(Paths.get(filePath))

    val fWriter = new PrintWriter(new FileOutputStream(new File(filePath), true))

    if(!exists) {
      val header = "number, solution, timestamp, duration, durationTuning, durationLowering, samples, executions, runtime" + "\n"
      fWriter.write(header)
    }

    val runtime = tuningResultStatistic.runtime match {
      case Some(value) => value.value.toString
      case None => "-1"
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

    fWriter.write(line)

    fWriter.close()
  }
}
