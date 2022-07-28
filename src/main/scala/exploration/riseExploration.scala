package exploration

import java.nio.file.{Files, Paths}
import elevate.core.Strategy
import elevate.core.strategies.traversal.topDown
import rise.elevate.rules.algorithmic.fuseReduceMap
import rise.elevate.rules.traversal.default

import scala.collection.immutable
import rise.elevate.strategies.normalForm.DFNF
import elevate.heuristic_search.{Metaheuristic, Runner}
import elevate.heuristic_search.util.{Solution, SolutionStep}
import elevate.core._
import elevate.core.strategies.basic._
import rise.autotune.HostCode
import rise.elevate.Rise
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.traversal._

import exploration.explorationUtil.jsonParser._
import exploration.runner._

import elevate.heuristic_search._

import scala.sys.process._
import scala.language.postfixOps

object riseExploration {

  // entry point for exploration
  def apply(expression: Rise,
            lowering: Strategy[Rise],
            strategies: scala.collection.immutable.Seq[Strategy[Rise]],
            filePath: String,
            hostCode: Option[HostCode] = None,
            rewriteFunction: Option[Solution[Rise] => scala.collection.immutable.Seq[Solution[Rise]]] = None,
            afterRewrite: Option[Strategy[Rise]] = None,
            importExport: Option[(String => Solution[Rise], (Solution[Rise], String) => Unit)] = None,
            expert: Option[Double] = None
           )
  : ExplorationResult[Rise] = {

    // parse config file
    val parsedConfiguration = exploration.explorationUtil.jsonParser.parse(filePath)

    // setup gold
    // code here

    val startingPoint = prepareExploration(
      parsedConfiguration,
      expression,
      lowering,
      strategies,
      filePath,
      hostCode,
      rewriteFunction = rewriteFunction,
      afterRewrite = afterRewrite,
      importExport = importExport,
      expert = expert
    )


    val solution = Solution[Rise](
      solutionSteps = scala.collection.immutable.Seq(
        SolutionStep[Rise](
          expression = expression,
          strategy = elevate.core.strategies.basic.id[Rise], //
          location = 0
        )
      )
    )

    // start
    val result = startingPoint.execute(solution)

    // collect results
    // code here

    result
  }

  // todo command line parser (replace apply function by main)
  // def main(args: Array[String], solution:Rise, filePath:String):Unit = {

  def prepareExploration(result: ParseExploration,
                         solution: Rise,
                         lowering: Strategy[Rise],
                         strategies: scala.collection.immutable.Seq[Strategy[Rise]],
                         filePath: String,
                         hostCode: Option[HostCode],
                         rewriteFunction: Option[Solution[Rise] => scala.collection.immutable.Seq[Solution[Rise]]] = None,
                         afterRewrite: Option[Strategy[Rise]],
                         importExport: Option[(String => Solution[Rise], (Solution[Rise], String) => Unit)] = None,
                         expert: Option[Double]
                        ): Metaheuristic[Rise] = {

    // -- todo --check elements -> requirements

    // now traverse result and create elements
    val inputSize = result.inputSize
    val name = result.name
    val output = result.output

    // maybe print json information

    // stick exploration together

    // read input expression
    // -- todo --  read expression from file

    // make this more generic
    //    val lowering = fuseReduceMap `@` everywhere `;` lowerToC
    //    val lowering = exploration.strategies.convolutionStrategies.loweringStrategy
    //    val lowering = exploration.strategies.scalStrategies.lowering
    //    val lowerings = exploration.strategies.scalStrategies.lowerings
    // add lowering for scal

    // use set

    // initialize gold expression
    // check, if this will work, if first expression can't be lowered properly
    val gold = lowering(solution).get

    // create unique output folder
    val uniqueFilename_full = uniqueFilename(output + "/" + name)
    (s"mkdir ${uniqueFilename_full}" !!)

    // copy configuration file to output folder
    (s"cp ${filePath} ${uniqueFilename_full}" !!)

    // create names
    val nameList = scala.collection.mutable.ListBuffer.empty[String]
    var predecessor = uniqueFilename_full
    result.metaheuristic.foreach(elem => {
      predecessor = predecessor + "/" + elem.heuristic
      nameList += predecessor
    })

    // create folder for executor
    val executorOutput = predecessor + "/" + "Executor"

    // create subfolders
    nameList.foreach(elem => {
      println("elem: " + elem)
      (s"mkdir ${elem}" !!)
      (s"mkdir ${elem}" + "/Expressions" !!)
    })

    // create subfolder for executor
    println("elem: " + executorOutput)
    (s"mkdir ${executorOutput}" !!)
    //    (s"mkdir ${executorOutput + "/C"}" !!)
    //    (s"mkdir ${executorOutput + "/lowered"}" !!)

    // begin with executor
    val executor = result.executor.name match {
      case "C" =>
        new CExecutor(
          lowering,
          gold,
          result.executor.iterations,
          inputSize,
          result.executor.threshold,
          executorOutput,
          expert = expert
        )
      case "AutoTuning" => new AutoTuningExecutor(lowering, gold, hostCode, result.executor.iterations, inputSize, result.executor.threshold, executorOutput)
      case "Debug" => new DebugExecutor(lowering, gold, result.executor.iterations, inputSize, result.executor.threshold, executorOutput)
      case "OpenMP" => new Exception("executor option not yet implemented")
      case "OpenCL" => new Exception("executor option not yet implemented")
      case _ => new Exception("not a supported executor option")
    }

    var index = 0

    // root metaheuristic using executor as executor
    val rootChoice = result.metaheuristic.reverse.head

    println("result.executor: " + result.executor.name)
    println("executro?: " + executor)

    val rootMetaheuristic = new Metaheuristic[Rise](
      rootChoice.heuristic,
      getHeuristic(rootChoice.heuristic),
      rootChoice.depth,
      rootChoice.samples,
      1,
      executor.asInstanceOf[Runner[Rise]],
      strategies,
      nameList.reverse.apply(index),
      rewriteFunction = rewriteFunction,
      afterRewrite = afterRewrite,
      importExport = importExport
    )

    index = index + 1

    // iterate reverse direction
    var metaheuristic = rootMetaheuristic
    result.metaheuristic.reverse.tail.foreach(elem => {
      // new metaheuristic with last one as Runner
      metaheuristic = new Metaheuristic[Rise](
        elem.heuristic,
        getHeuristic(elem.heuristic),
        elem.depth,
        elem.samples,
        1,
        metaheuristic,
        strategies,
        nameList.reverse.apply(index),
        rewriteFunction = rewriteFunction,
        afterRewrite = afterRewrite,
        importExport = importExport
      )

      index = index + 1
    })

    metaheuristic
  }

  def uniqueFilename(path: String): String = {
    // check if output path already exists
    var uniqueFilename_full = path
    if (Files.exists(Paths.get(uniqueFilename_full))) {
      // wait for next millisecond to create unique filename using timestamp
      Thread.sleep(1)
      uniqueFilename_full = uniqueFilename_full + "_" + System.currentTimeMillis()
    }

    uniqueFilename_full
  }

}

