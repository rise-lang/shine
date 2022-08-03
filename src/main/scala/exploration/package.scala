import elevate.heuristic_search.util.{Solution, SolutionStep}
import rise.elevate.Rise
import rise.core._
import elevate.core.Strategy
import rise.autotune.{HostCode, search}
import elevate.heuristic_search._
import exploration.runner._
import exploration.neighborhoods._

import java.nio.file.{Files, Paths}
import scala.sys.process._
import scala.language.{existentials, postfixOps}

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
                       neighborhoodConfig: NeighborhoodConfig = null,
                       checkExpression: Option[Rise => Boolean] = None,
                       hostCode: Option[HostCode] = None, // hostcode to execute
                       rewriteFunction: Option[Solution[Rise] => scala.collection.immutable.Seq[Solution[Rise]]] = None,
                       normalForm: Option[Strategy[Rise]] = None, // apply normal form after each rewrite
                       importExport: Option[(String => Solution[Rise], (Solution[Rise], String) => Unit)] = None, // how to import/export a solution
                       expert: Option[Double] = None,
                       overwrite: Boolean = false,
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

  case class NeighborhoodConfig(
                                 neighborhood: NeighborhoodChoice = NTreeChildrenChoice,
                                 slideWindow: Int = 10 // size? // distance?
                               )

  def explore(explorer: Explorer)(expression: Expr)
  : ExplorationResult[Rise] = {


    // make output
    (s"mkdir -p ${explorer.output}" !!)

    explorer.metaheuristics match {
      case Left(config) => {

        // create output parent folder (if not existent)
        (s"mkdir -p ${explorer.output}" !!)

        val uniqueFilename_full = explorer.overwrite match {
          case false => uniqueFilename(explorer.output + "/" + explorer.name)
          case true => explorer.output + "/" + explorer.name
        }

        (s"mkdir -p ${uniqueFilename_full}/hm" !!)
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

          val solution = Solution[Rise](
            solutionSteps = scala.collection.immutable.Seq(
              SolutionStep[Rise](
                expression = expression,
                strategy = elevate.core.strategies.basic.id[Rise], //
                location = 0
              )
            )
          )

          entryPoint.execute(solution)

          (s"cp ${uniqueFilename_full}/${config.reverse.last.heuristic}_${iteration}/${config.reverse.last.heuristic}_hm_${iteration}.csv ${uniqueFilename_full}/hm" !!)
          (s"cp ${uniqueFilename_full}/${config.reverse.last.heuristic}_${iteration}/${config.reverse.last.heuristic}_hm_${iteration}.csv ${uniqueFilename_full}/csv" !!)

          // copy tuningStatistics.json

          (s"cp ${uniqueFilename_full}/${config.reverse.last.heuristic}_${iteration}/Executor/tuningStatistics.json ${uniqueFilename_full}" !!) // should not matter
        }

        // plot total
        // directory uniqueFilename_full
        // plot
        val plot = explorer.expert match {
          case Some(value) => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/hm -l ${config.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --plot_log --exp ${value}"
          case None => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/hm -l ${config.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --plot_log"
        }
        val plot_log = explorer.expert match {
          case Some(value) => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/hm -l ${config.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}_log.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --exp ${value}"
          case None => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/hm -l ${config.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}_log.pdf --y_label 'Runtime(ms)' --title ${explorer.name} "
        }

        (plot !!)
        (plot_log !!)


        // dummy output therefore id strategy
        val solutionResult = Solution[Rise](
          solutionSteps = scala.collection.immutable.Seq(
            SolutionStep[Rise](
              expression = expression,
              strategy = elevate.core.strategies.basic.id[Rise],
              location = 0
            )
          )
        )


        ExplorationResult(
          solution = solutionResult,
          performance = None,
          searchSpace = None
        )


      }
      case Right(config) => {

        // run stuff in loop

        // mkdir on top

        // create output parent folder (if not existent)
        (s"mkdir -p ${explorer.output}" !!)

        val uniqueFilenameRoot = explorer.overwrite match {
          case false => uniqueFilename(explorer.output + "/" + explorer.name)
          case true => explorer.output + "/" + explorer.name
        }

        //        val uniqueFilenameRoot = uniqueFilename(explorer.output + "/" + explorer.name)

        config.foreach(metaheuristic => {


          val methodName = metaheuristic.reverse.last.heuristic

          // don't overwrite methods
          val uniqueFilename_full = uniqueFilename(uniqueFilenameRoot + "/" + methodName)

          (s"mkdir -p ${uniqueFilename_full}/hm" !!)
          (s"mkdir -p ${uniqueFilename_full}/csv" !!)

          for (iteration <- Range(0, metaheuristic.reverse.last.repeat)) {
            println("iteration: " + iteration)

            //            val jsonPath = metaheuristic.map(elem => elem.heuristic + "_" + iteration.toString).mkString("/")
            //            println("jsonPath: " + jsonPath)

            //            System.exit(0)

            // start rise exploration here!
            val entryPoint = prepareExploration(
              expression,
              explorer,
              uniqueFilename_full,
              metaheuristic,
              iteration
            )

            // todo make this generic
            // add solution on leaf layer

            // make space for id strategy
            var steps = scala.collection.mutable.Seq.empty[SolutionStep[Rise]]
            Range(0, entryPoint.depth + 1).foreach(elem => {
              steps = steps :+ SolutionStep[Rise](
                expression = expression,
                strategy = elevate.core.strategies.basic.id[Rise], //
                location = 0
              )
            })

            val solution = Solution[Rise](
              solutionSteps = steps.toSeq
            )

            //            val solution = Solution[Rise](
            //              solutionSteps = scala.collection.immutable.Seq(
            //                SolutionStep[Rise](
            //                  expression = expression,
            //                  strategy = elevate.core.strategies.basic.id[Rise], //
            //                  location = 0
            //                )
            //              )
            //            )

            //            entryPoint.execute(Solution(expression, Seq.empty[Strategy[Rise]]))
            entryPoint.execute(solution)

            (s"cp ${uniqueFilename_full}/${metaheuristic.reverse.last.heuristic}_${iteration}/${metaheuristic.reverse.last.heuristic}_hm_${iteration}.csv ${uniqueFilename_full}/hm" !!)
            (s"cp ${uniqueFilename_full}/${metaheuristic.reverse.last.heuristic}_${iteration}/${metaheuristic.reverse.last.heuristic}_${iteration}.csv ${uniqueFilename_full}/csv" !!)

            // copy json
            val jsonPath = metaheuristic.map(elem => elem.heuristic + "_" + iteration.toString).mkString("/")
            (s"cp ${uniqueFilename_full}/${jsonPath}/Executor/tuningStatistics.json ${uniqueFilename_full}" !!) // should not matter
          }

          // plot total
          // directory uniqueFilename_full
          // plot
          val plot = explorer.expert match {
            case Some(value) => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/hm -l ${metaheuristic.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --plot_log --exp ${value}"
            case None => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/hm -l ${metaheuristic.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --plot_log"
          }
          val plot_log = explorer.expert match {
            case Some(value) => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/hm -l ${metaheuristic.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}_log.pdf --y_label 'Runtime(ms)' --title ${explorer.name} --exp ${value}"
            case None => s"hm-plot-optimization-results -j ${uniqueFilename_full}/tuningStatistics.json -i ${uniqueFilename_full}/hm -l ${metaheuristic.reverse.last.heuristic} -o ${uniqueFilename_full}/${explorer.name}_log.pdf --y_label 'Runtime(ms)' --title ${explorer.name} "
          }

          (plot !!)
          (plot_log !!)

        })

        plot_experiment(uniqueFilenameRoot, config, explorer)
        plot_experiment2(uniqueFilenameRoot, explorer)

        // todo it might be necessary to call this during a search
        // reset counter
        rise.core.freshName.reset()

        val solution = Solution[Rise](
          solutionSteps = scala.collection.immutable.Seq(
            SolutionStep[Rise](
              expression = expression,
              strategy = elevate.core.strategies.basic.id[Rise], //
              location = 0
            )
          )
        )

        ExplorationResult(
          solution = solution,
          performance = None,
          searchSpace = None
        )

      }
    }

  }

  def plot_experiment2(uniqueFilenameRoot: String, explorer: Explorer) = {
    val command = s"python experiment/plot_experiment2.py ${uniqueFilenameRoot}"

    println("command: " + command)

    command !!

  }


  def plot_experiment(uniqueFilenameRoot: String, config: Seq[Seq[MetaheuristicConfig]], explorer: Explorer) = {

    val configFile = uniqueFilenameRoot + "/" + config.last.reverse.last.heuristic + "/" + "tuningStatistics.json"
    var folders = ""
    var names = ""
    config.foreach(elem => {
      folders += uniqueFilenameRoot + "/" + elem.reverse.last.heuristic + "/" + "hm" + " "
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
      predecessor = predecessor + "/" + elem.heuristic + "_" + iteration
      nameList += predecessor
    })

    // create folder for executor
    val executorOutput = predecessor + "/" + "Executor"
    //    val executorOutput = predecessor + "_" + iteration + "/" + "Executor"
    //    val executorOutput = predecessor + "/" + "Executor"

    println("NameList: \n")
    nameList.foreach(println)
    println("\n")

    // create subfolders
    nameList.foreach(elem => {
      println("elem: " + elem)
      //      (s"mkdir ${elem}_${iteration}" !!)
      //      (s"mkdir ${elem}_${iteration}" + "/Expressions" !!)
      (s"mkdir ${elem}" !!)
      (s"mkdir ${elem}" + "/Expressions" !!)
    })


    // create subfolder for executor
    println("elem: " + executorOutput)
    (s"mkdir ${executorOutput}" !!)

    //    System.exit(0)

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

    val neighborhood: HeuristicPanel[Rise] = explorer.neighborhoodConfig.neighborhood match {
      case neighborhoods.NTreeChildrenChoice =>
        NTreeChildren(
          runner = executor.asInstanceOf[Runner[Rise]],
          strategies = explorer.strategies,
          afterRewrite = None,
          checkExpression = explorer.checkExpression
        )
      case neighborhoods.NTreeChildrenParentChoice =>
        NTreeChildrenParent(
          runner = executor.asInstanceOf[Runner[Rise]],
          strategies = explorer.strategies,
          afterRewrite = None,
          checkExpression = explorer.checkExpression
        )
      case neighborhoods.NTreeLeafsWindowChoice =>
        NTreeLeafsWindow(
          runner = executor.asInstanceOf[Runner[Rise]],
          strategies = explorer.strategies,
          afterRewrite = None,
          checkExpression = explorer.checkExpression
        )
      case neighborhoods.NTreeLeafsDistanceChoice => throw new Exception("not yet implemented")
      case neighborhoods.NPathDistanceChoice => throw new Exception("not yet implemented")
      case neighborhoods.NGraphChoice => throw new Exception("not yet implemented")
    }

    // root metaheuristic using executor as executor
    val rootChoice = metaheuristics.reverse.head

    val rootMetaheuristic = new Metaheuristic[Rise](
      name = rootChoice.heuristic,
      heuristic = exploration.explorationUtil.jsonParser.getHeuristic(rootChoice.heuristic),
      depth = rootChoice.depth,
      samples = rootChoice.samples,
      repetitions = rootChoice.repetitions,
      runner = executor.asInstanceOf[Runner[Rise]],
      strategies = explorer.strategies,
      output = nameList.reverse.apply(index),
      rewriteFunction = explorer.rewriteFunction,
      afterRewrite = explorer.normalForm,
      importExport = explorer.importExport,
      heuristicPanel = Some(neighborhood),
      iteration = Some(iteration)
    )

    index = index + 1

    // iterate reverse direction
    var metaheuristic = rootMetaheuristic
    metaheuristics.reverse.tail.foreach(elem => {


      val neighborhood: HeuristicPanel[Rise] = explorer.neighborhoodConfig.neighborhood match {
        case neighborhoods.NTreeChildrenChoice =>
          NTreeChildren(
            runner = metaheuristic,
            strategies = explorer.strategies,
            afterRewrite = None,
            checkExpression = explorer.checkExpression
          )
        case neighborhoods.NTreeChildrenParentChoice =>
          NTreeChildrenParent(
            runner = metaheuristic,
            strategies = explorer.strategies,
            afterRewrite = None,
            checkExpression = explorer.checkExpression
          )
        case neighborhoods.NTreeLeafsWindowChoice =>
          NTreeLeafsWindow(
            runner = metaheuristic,
            strategies = explorer.strategies,
            slideWindow = explorer.neighborhoodConfig.slideWindow, // e.g. distance
            afterRewrite = None,
            checkExpression = explorer.checkExpression
          )
        case neighborhoods.NTreeLeafsDistanceChoice => throw new Exception("not yet implemented")
        case neighborhoods.NPathDistanceChoice => throw new Exception("not yet implemented")
        case neighborhoods.NGraphChoice => throw new Exception("not yet implemented")
      }

      // new metaheuristic with last one as Runner
      metaheuristic = new Metaheuristic[Rise](
        name = elem.heuristic,
        heuristic = exploration.explorationUtil.jsonParser.getHeuristic(elem.heuristic),
        depth = elem.depth,
        samples = elem.samples,
        repetitions = elem.repetitions,
        runner = metaheuristic,
        strategies = explorer.strategies,
        output = nameList.reverse.apply(index),
        rewriteFunction = explorer.rewriteFunction,
        afterRewrite = explorer.normalForm,
        importExport = explorer.importExport,
        heuristicPanel = Some(neighborhood),
        iteration = Some(iteration)
      )

      index = index + 1
    })

    metaheuristic
  }

  def uniqueFilename(path: String): String = {

    // check if output path already exists
    val uniquePath = Files.exists(Paths.get(path)) match {
      case true =>
        // check if path already ends with "_number"
        val isCountedFormat = ".*_[0123456789]+$".r.findFirstIn(path)
        isCountedFormat match {
          case Some(_) =>
            val pathSplit: Seq[String] = path.split("_").toSeq
            val number: Int = pathSplit.last.toInt + 1
            val countedPath: String = pathSplit.slice(0, pathSplit.size - 1).mkString("_") + "_" + number.toString
            uniqueFilename(countedPath)
          case None =>
            val uniquePathCandidate = path + "_" + "0"
            uniqueFilename(uniquePathCandidate)
        }
      case false =>
        path
    }
    uniquePath
  }
}
