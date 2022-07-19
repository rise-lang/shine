import elevate.heuristic_search.util.Solution
import rise.elevate.Rise
import rise.core._
import elevate.core.Strategy
import rise.autotune.{HostCode, search}
import elevate.heuristic_search._
import exploration.runner._

import java.nio.file.{Files, Paths}
import scala.sys.process._
import scala.language.postfixOps

package object exploration {

  case class Explorer(
                       name: String = "exploration",
                       output: String = "exploration",
                       inputSize: Int = 1024,
                       metaheuristics: Either[Seq[MetaheuristicConfig], Seq[Seq[MetaheuristicConfig]]] = null,
                       executor: ExecutorConfig = null,
                       lowering: Strategy[Rise] = null,
                       strategies: scala.collection.immutable.Seq[Strategy[Rise]] = null,
                       printEvery: Int = 100,
                       //optional
                       hostCode: Option[HostCode] = None, // hostcode to execute
                       rewriteFunction: Option[Solution[Rise] => scala.collection.immutable.Seq[Solution[Rise]]] = null,
                       normalForm: Option[Strategy[Rise]] = None, // apply normal form after each rewrite
                       importExport: Option[(String => Solution[Rise], (Solution[Rise], String) => Unit)] = None, // how to import/export a solution
                       expert: Option[Double] = None,
                       //                       repeat: Int = 1
                     )

  case class MetaheuristicConfig(
                                  heuristic: String,
                                  depth: Int,
                                  samples: Int = 100, // todo make this an option
                                  repetitions: Int = 1, // usually 1
                                  repeat: Int = 1
                                )

  case class ExecutorConfig(
                             name: String,
                             iterations: Int,
                             threshold: Double
                           )


  def explore(explorer: Explorer)(expression: Expr)
  : ExplorationResult[Rise] = {

    explorer.metaheuristics match {
      case Left(config) => {

        // create output parent folder (if not existent)
        (s"mkdir -p ${explorer.output}" !!)

        val uniqueFilename_full = uniqueFilename(explorer.output + "/" + explorer.name)

        (s"mkdir -p ${uniqueFilename_full}/csv" !!)

        for (iteration <- Range(0, config.reverse.last.repeat)) {
          println("iteration: " + iteration)


          // start rise exploration here!
          val entryPoint = prepareExploration(
            expression,
            explorer,
            uniqueFilename_full,
            config,
            iteration
          )

          entryPoint.execute(Solution(expression, Seq.empty[Strategy[Rise]]))

          (s"cp ${uniqueFilename_full}/${config.reverse.last.heuristic}_${iteration}/${config.reverse.last.heuristic}_${iteration}.csv ${uniqueFilename_full}/csv" !!)
          (s"cp ${uniqueFilename_full}/${config.reverse.last.heuristic}_${iteration}/Executor/tuningStatistics.json ${uniqueFilename_full}" !!) // should not matter
        }

        // plot total
        // directory uniqueFilename_full
        // plot
        val plot = explorer.expert match {
          case Some(value) => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/csv -l ${config.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --plot_log --exp ${value}"
          case None => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/csv -l ${config.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --plot_log"
        }
        val plot_log = explorer.expert match {
          case Some(value) => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/csv -l ${config.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}_log.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --exp ${value}"
          case None => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/csv -l ${config.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}_log.pdf --y_label 'Runtime(ms)' --title ${explorer.name} "
        }

        (plot !!)
        (plot_log !!)


        ExplorationResult(
          solution = Solution(expression, strategies = explorer.strategies),
          performance = None,
          searchSpace = None
        )


      }
      case Right(config) => {

        // run stuff in loop

        // mkdir on top

        // create output parent folder (if not existent)
        (s"mkdir -p ${explorer.output}" !!)
        val uniqueFilenameRoot = uniqueFilename(explorer.output + "/" + explorer.name)

        config.foreach(metaheuristic => {
          val methodName = metaheuristic.reverse.last.heuristic

          val uniqueFilename_full = uniqueFilenameRoot + "/" + methodName

          (s"mkdir -p ${uniqueFilename_full}/csv" !!)

          for (iteration <- Range(0, metaheuristic.reverse.last.repeat)) {
            println("iteration: " + iteration)

            // start rise exploration here!
            val entryPoint = prepareExploration(
              expression,
              explorer,
              uniqueFilename_full,
              metaheuristic,
              iteration
            )

            entryPoint.execute(Solution(expression, Seq.empty[Strategy[Rise]]))

            (s"cp ${uniqueFilename_full}/${metaheuristic.reverse.last.heuristic}_${iteration}/${metaheuristic.reverse.last.heuristic}_${iteration}.csv ${uniqueFilename_full}/csv" !!)
            (s"cp ${uniqueFilename_full}/${metaheuristic.reverse.last.heuristic}_${iteration}/Executor/tuningStatistics.json ${uniqueFilename_full}" !!) // should not matter
          }

          // plot total
          // directory uniqueFilename_full
          // plot
          val plot = explorer.expert match {
            case Some(value) => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/csv -l ${metaheuristic.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --plot_log --exp ${value}"
            case None => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/csv -l ${metaheuristic.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --plot_log"
          }
          val plot_log = explorer.expert match {
            case Some(value) => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/csv -l ${metaheuristic.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}_log.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --exp ${value}"
            case None => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/csv -l ${metaheuristic.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}_log.pdf --y_label 'Runtime(ms)' --title ${explorer.name} "
          }

          (plot !!)
          (plot_log !!)

        })

        plot_experiment(uniqueFilenameRoot, config, explorer)

        ExplorationResult(
          solution = Solution(expression, strategies = explorer.strategies),
          performance = None,
          searchSpace = None
        )

      }
    }

  }

  def plot_experiment(uniqueFilenameRoot: String, config: Seq[Seq[MetaheuristicConfig]], explorer: Explorer) = {

    val configFile = uniqueFilenameRoot + "/" + config.last.reverse.last.heuristic + "/" + "tuningStatistics.json"
    var folders = ""
    var names = ""
    config.foreach(elem => {
      folders += uniqueFilenameRoot + "/" + elem.reverse.last.heuristic + "/" + "csv" + " "
      names += elem.reverse.last.heuristic + " "
    })

    val exp: String = explorer.expert match {
      case Some(value) => s"--exp ${value} "
      case None => " "
    }

    val plot_command =
      "hm-plot-optimization-results " +
        s"-j ${configFile} " +
        "-i " +
        folders +
        "-l " +
        names +
        s"-o ${uniqueFilenameRoot}/${explorer.name}.pdf " +
        "--y_label \"Log Runtime(ms)\" " +
        exp +
        s"--title ${explorer.name} "

    val plot_log_command =
      "hm-plot-optimization-results " +
        s"-j ${configFile} " +
        "-i " +
        folders +
        "-l " +
        names +
        s"-o ${uniqueFilenameRoot}/${explorer.name}_log.pdf " +
        "--plot_log " +
        "--y_label \"Log Runtime(ms)\" " +
        exp +
        s"--title ${explorer.name} "


    println("command: " + plot_command)
    plot_command !!

    println("command: " + plot_command)
    plot_log_command !!

  }


  // todo cleanup
  def prepareExploration(
                          expression: Expr,
                          explorer: Explorer,
                          uniqueFilename_full: String,
                          metaheuristics: Seq[MetaheuristicConfig],
                          iteration: Int
                        )
  : Metaheuristic[Rise] = {

    // initialize gold expression ( we expect lowering to work)
    val gold = explorer.lowering(expression).get

    // create output parent folder (if not existent)
    (s"mkdir -p ${explorer.output}" !!)

    // create unique output folder
    (s"mkdir -p ${uniqueFilename_full}" !!)

    // copy configuration file to output folder if provided
    //    (s"cp ${explorer.output} ${uniqueFilename_full}" !!)

    // create names
    val nameList = scala.collection.mutable.ListBuffer.empty[String]
    var predecessor = uniqueFilename_full
    metaheuristics.foreach(elem => {
      predecessor = predecessor + "/" + elem.heuristic
      nameList += predecessor
    })


    // create folder for executor
    val executorOutput = predecessor + "_" + iteration + "/" + "Executor"
    //    val executorOutput = predecessor + "/" + "Executor"

    // create subfolders
    nameList.foreach(elem => {
      println("elem: " + elem)
      (s"mkdir ${elem}_${iteration}" !!)
      (s"mkdir ${elem}_${iteration}" + "/Expressions" !!)
    })

    // create subfolder for executor
    println("elem: " + executorOutput)
    (s"mkdir ${executorOutput}" !!)

    // begin with executor
    val executor = explorer.executor.name match {
      case "C" => new CExecutor(
        explorer.lowering,
        gold,
        explorer.executor.iterations,
        explorer.inputSize,
        explorer.executor.threshold,
        executorOutput,
        printEvery = explorer.printEvery,
        expert = explorer.expert
      )
      case "AutoTuning" => new AutoTuningExecutor(explorer.lowering, gold, explorer.hostCode, explorer.executor.iterations, explorer.inputSize, explorer.executor.threshold, executorOutput)
      case "Debug" => new DebugExecutor(explorer.lowering, gold, explorer.executor.iterations, explorer.inputSize, explorer.executor.threshold, executorOutput)
      case "OpenMP" => new Exception("executor option not yet implemented")
      case "OpenCL" => new Exception("executor option not yet implemented")
      case _ => new Exception("not a supported executor option")
    }

    var index = 0

    // root metaheuristic using executor as executor
    val rootChoice = metaheuristics.reverse.head

    val rootMetaheuristic = new Metaheuristic[Rise](
      rootChoice.heuristic,
      exploration.explorationUtil.jsonParser.getHeuristic(rootChoice.heuristic),
      rootChoice.depth,
      rootChoice.samples,
      rootChoice.repetitions,
      executor.asInstanceOf[Runner[Rise]],
      explorer.strategies,
      nameList.reverse.apply(index),
      rewriteFunction = explorer.rewriteFunction,
      afterRewrite = explorer.normalForm,
      importExport = explorer.importExport,
      iteration = Some(iteration)
    )

    index = index + 1

    // iterate reverse direction
    var metaheuristic = rootMetaheuristic
    metaheuristics.reverse.tail.foreach(elem => {
      // new metaheuristic with last one as Runner
      metaheuristic = new Metaheuristic[Rise](
        elem.heuristic,
        exploration.explorationUtil.jsonParser.getHeuristic(elem.heuristic),
        elem.depth,
        elem.samples,
        elem.repetitions,
        metaheuristic,
        explorer.strategies,
        nameList.reverse.apply(index),
        rewriteFunction = explorer.rewriteFunction,
        afterRewrite = explorer.normalForm,
        importExport = explorer.importExport,
        iteration = Some(iteration)
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
