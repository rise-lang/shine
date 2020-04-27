package exploration

import java.nio.file.{Files, Paths}

import exploration.runner.CExecutor
import elevate.heuristic_search.Metaheuristic
import elevate.rise.{Rise}
import exploration.util.{jsonParser}
import exploration.util.jsonParser.ParseExploration
import strategies.standardStrategies

import scala.sys.process._
import scala.language.postfixOps


object riseExploration {


  // entry point for exploration
  def apply(solution:Rise, filePath:String) = {

    // parse config file
    val parsedConfiguration = jsonParser.parse(filePath)

    // setup gold
    // code here

    val startingPoint = prepareExploration(parsedConfiguration, solution)

    // start
    startingPoint.execute(solution)

    // collect results
    // code here

  }

    // todo command line parser (replace apply function by main)
//    def main(args: Array[String], solution:Rise, filePath:String):Unit = {

  def prepareExploration(result: ParseExploration, solution:Rise):Metaheuristic[Rise] = {

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
    val lowering = elevate.rise.rules.lowering.lowerToC
    val gold = lowering.apply(solution).get

    // create unique output folder
    val uniqueFilename_full = uniqueFilename(output + "/" + name)
    (s"mkdir ${uniqueFilename_full}" !!)

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
    (s"mkdir ${executorOutput + "/C"}" !!)
    (s"mkdir ${executorOutput + "/lowered"}" !!)

    // begin with executor
    val executor = result.executor.name match {
      case "C" => new CExecutor(lowering, gold, result.executor.iterations, inputSize, result.executor.threshold, executorOutput)
      case "OpenMP" => new Exception("executor option not yet implemented")
      case "OpenCL" => new Exception("executor option not yet implemented")
      case _ => new Exception("not a supported executor option")
    }

    var index = 0

    // root metaheuristic using executor as executor
    val rootChoice = result.metaheuristic.reverse.head
    val rootMetaheuristic = new Metaheuristic[Rise](rootChoice.heuristic, jsonParser.getHeuristic(rootChoice.heuristic),
      rootChoice.depth,rootChoice.iteration, executor.asInstanceOf[CExecutor], standardStrategies.strategies, nameList.reverse.apply(index))
    index = index + 1

    // iterate reverse direction
    var metaheuristic = rootMetaheuristic
    result.metaheuristic.reverse.tail.foreach(elem => {
      // new metaheuristic with last one as Runner
      metaheuristic = new Metaheuristic[Rise](elem.heuristic, jsonParser.getHeuristic(elem.heuristic),
        elem.depth,elem.iteration, metaheuristic, standardStrategies.strategies, nameList.reverse.apply(index))
      index = index + 1
    })

    metaheuristic
  }

  def uniqueFilename(path:String):String = {
    // check if output path already exists
    var uniqueFilename_full = path
    if(Files.exists(Paths.get(uniqueFilename_full))){
      println("path is there")
      val warningString = "Warning! Clash at " + uniqueFilename_full + ".\n"
      println(warningString + "adding System.currentTimeMillis().")
      uniqueFilename_full = uniqueFilename_full + "_" + System.currentTimeMillis()
    }

    uniqueFilename_full
  }

}

