import elevate.heuristic_search.util.Solution
import rise.elevate.Rise
import rise.core._
import elevate.core.Strategy
import rise.autotune.HostCode


import elevate.heuristic_search.{Metaheuristic, Runner}

import exploration.runner._

import java.nio.file.{Files, Paths}
import scala.sys.process._
import scala.language.postfixOps

package object exploration {

  case class Explorer(
                       name: String = "exploration",
                       output: String = "exploration",
                       inputSize: Int = 1024,
                       metaheuristics: Seq[MetaheuristicConfig] = null,
                       executor: ExecutorConfig = null,
                       lowering: Strategy[Rise] = null,
                       strategies: Set[Strategy[Rise]] = null,
                       //optional
                       hostCode: Option[HostCode] = None, // hostcode to execute
                       rewriteFunction: Option[Solution[Rise] => Set[Solution[Rise]]] = null,
                       normalForm: Option[Strategy[Rise]] = None, // apply normal form after each rewrite
                       importExport: Option[(String => Solution[Rise], (Solution[Rise], String) => Unit)] = None // how to import/export a solution
                     )

  case class MetaheuristicConfig(
                                  heuristic: String,
                                  depth: Int,
                                  iteration: Int
                                )

  case class ExecutorConfig(
                             name: String,
                             iterations: Int,
                             threshold: Double
                           )


  // todo add elements expected in exploration result
  case class ExplorationResult(

                              ) {
    override def toString: String = {
      // print information here
      "ExplorationResult: "
    }
  }


  def explore(explorer: Explorer)(expression: Expr)
  //  : ExplorationResult = {
  : (Rise, Option[Double]) = {


    // start rise exploration here!
    val entryPoint = prepareExploration(
      expression,
      explorer
    )


    entryPoint.execute(Solution(expression, Seq.empty[Strategy[Rise]]))
  }


  // todo cleanup
  def prepareExploration(
                          expression: Expr,
                          explorer: Explorer
                        )
  : Metaheuristic[Rise] = {

    // initialize gold expression ( we expect lowering to work)
    val gold = explorer.lowering(expression).get

    // create output parent folder (if not existent)
    (s"mkdir -p ${explorer.output}" !!)

    // create unique output folder
    val uniqueFilename_full = uniqueFilename(explorer.output + "/" + explorer.name)
    (s"mkdir ${uniqueFilename_full}" !!)

    // copy configuration file to output folder if provided
    //    (s"cp ${explorer.output} ${uniqueFilename_full}" !!)

    // create names
    val nameList = scala.collection.mutable.ListBuffer.empty[String]
    var predecessor = uniqueFilename_full
    explorer.metaheuristics.foreach(elem => {
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

    // begin with executor
    val executor = explorer.executor.name match {
      case "C" => new CExecutor(explorer.lowering, gold, explorer.executor.iterations,
        explorer.inputSize, explorer.executor.threshold, executorOutput)
      case "AutoTuning" => new AutoTuningExecutor(explorer.lowering, gold, explorer.hostCode, explorer.executor.iterations, explorer.inputSize, explorer.executor.threshold, executorOutput)
      case "Debug" => new DebugExecutor(explorer.lowering, gold, explorer.executor.iterations, explorer.inputSize, explorer.executor.threshold, executorOutput)
      case "OpenMP" => new Exception("executor option not yet implemented")
      case "OpenCL" => new Exception("executor option not yet implemented")
      case _ => new Exception("not a supported executor option")
    }

    var index = 0

    // root metaheuristic using executor as executor
    val rootChoice = explorer.metaheuristics.reverse.head

    val rootMetaheuristic = new Metaheuristic[Rise](
      rootChoice.heuristic,
      exploration.explorationUtil.jsonParser.getHeuristic(rootChoice.heuristic),
      rootChoice.depth,
      rootChoice.iteration,
      executor.asInstanceOf[Runner[Rise]],
      explorer.strategies,
      nameList.reverse.apply(index),
      rewriteFunction = explorer.rewriteFunction,
      afterRewrite = explorer.normalForm,
      importExport = explorer.importExport
    )

    index = index + 1

    // iterate reverse direction
    var metaheuristic = rootMetaheuristic
    explorer.metaheuristics.reverse.tail.foreach(elem => {
      // new metaheuristic with last one as Runner
      metaheuristic = new Metaheuristic[Rise](
        elem.heuristic,
        exploration.explorationUtil.jsonParser.getHeuristic(elem.heuristic),
        elem.depth,
        elem.iteration,
        metaheuristic,
        explorer.strategies,
        nameList.reverse.apply(index),
        rewriteFunction = explorer.rewriteFunction,
        afterRewrite = explorer.normalForm,
        importExport = explorer.importExport
      )

      index = index + 1
    })

    metaheuristic
  }

  // todo this occurs multiple times in code
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
