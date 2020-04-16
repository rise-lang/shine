package exploration.util

import java.nio.file.{Files, Paths}
import elevate.heuristic_search.Heuristic
import elevate.heuristic_search.heuristics.Random
import exploration.search.CExecutor
import elevate.core.strategies.traversal.{oncebu, oncetd}
import elevate.core.{Strategy}
import elevate.heuristic_search.{Metaheuristic}
import elevate.heuristic_search.heuristic.IterativeImprovement
import elevate.rise.rules.algorithmic.{blockedReduce, fissionReduceMap, fuseReduceMap}
import elevate.rise.rules.movement.{liftReduce, mapFBeforeSlide}
import elevate.rise.{Rise, rules}
import elevate.rise.rules.traversal.LiftTraversable
import elevate.rise.strategies.normalForm.{LCNF, RNF}
import elevate.rise.strategies.tiling
import elevate.rise.strategies.tiling.tileNDList

import rise.core.TypedDSL.{add, fst, fun, l, map, reduce, snd, transpose, zip}
import rise.core.types.{ArrayType, f32, infer}

import scala.io._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.sys.process._
import scala.language.postfixOps

// -- todo -- move this to rise exploration
// just wip solution
// create new location encoding all strategy options
object Strategies {
  // blocking strategy: to be moved!

  // prepare tiling, keep reduce(+) and map(*) close together, (necessary?)
  val fusedReduceMap: Strategy[Rise] = LCNF `;` oncetd(fuseReduceMap)

  // M.N.m.n.K (or in TVM terms: xo,yo,xi,yi,k)
  val tiledOuterTwo: Strategy[Rise] = fusedReduceMap `;`
    oncetd(tileNDList(List(32,32))) `;` LCNF

  // M.N.m.n.K.k (tile K-loop),
  // fission first to enable blocking the reduction loop
  val splitK: Strategy[Rise] = tiledOuterTwo `;`
    oncetd(fissionReduceMap) `;` oncetd(blockedReduce(4)) `;` LCNF

  // move the split (blocking the reduction loop)
  // to prepare fusing map(*) and reduce(+) again
  val prepareFusion: Strategy[Rise] = splitK `;`
    oncetd(mapFBeforeSlide) `;` LCNF

  // move map(*) into both reduce loops again
  val fusedReduceMapAgain: Strategy[Rise] = prepareFusion `;`
    oncetd(fuseReduceMap) `;` LCNF `;` oncetd(fuseReduceMap) `;` LCNF

  // move outer K loop up M.N.m.K.n.k
  val moveOuterKLoopOnce: Strategy[Rise] = fusedReduceMapAgain `;`
    RNF `;` oncetd(liftReduce) `;` LCNF

  // move outer K loop further up M.N.K.m.n.k
  val moveOuterKLoopTwice: Strategy[Rise] = moveOuterKLoopOnce `;`
    RNF `;` oncetd(liftReduce) `;` LCNF

  // move inner K loop further up M.N.K.m.k.n,
  val moveInnerKLoopOnce: Strategy[Rise] = moveOuterKLoopTwice `;`
    RNF `;` oncebu(liftReduce) `;` LCNF

  val blocking : Strategy[Rise] = moveInnerKLoopOnce `;`
    RNF `;` oncebu(liftReduce)

  val strategies = Set(
    LCNF `;` blocking `;` LCNF,
    LCNF `;` rules.algorithmic.splitJoin(8) `;` LCNF,
    LCNF `;` rules.algorithmic.mapLastFission `;` LCNF,
    LCNF `;` rules.algorithmic.mapFusion `;` LCNF,
    LCNF `;` rules.algorithmic.liftId `;` LCNF,
    LCNF `;` rules.algorithmic.idAfter `;` LCNF,
    LCNF `;` rules.algorithmic.createTransposePair `;` LCNF,
    LCNF `;` rules.algorithmic.removeTransposePair `;` LCNF,
    LCNF `;` rules.algorithmic.slideSeqFusion `;` LCNF,
    LCNF `;` rules.movement.joinBeforeJoin `;` LCNF,
    LCNF `;` rules.movement.joinBeforeMapF `;` LCNF,
    LCNF `;` rules.movement.joinBeforeTranspose `;` LCNF,
    LCNF `;` rules.movement.mapFBeforeSlide `;` LCNF,
    LCNF `;` rules.movement.mapJoinBeforeJoin `;` LCNF,
    LCNF `;` rules.movement.mapJoinBeforeTranspose `;` LCNF,
    LCNF `;` rules.movement.mapTransposeBeforeJoin `;` LCNF,
    LCNF `;` rules.movement.transposeBeforeSlide `;` LCNF,
    LCNF `;` rules.movement.transposeBeforeMapMapF `;` LCNF,
    LCNF `;` rules.movement.transposeBeforeMapJoin `;` LCNF,
    LCNF `;` tiling.loopInterchange `;` LCNF,
    LCNF `;` tiling.tileND(32)(32) `;` LCNF,
    LCNF `;` tiling.tilingExternal `;` LCNF,
    LCNF `;` fusedReduceMap `;` LCNF,
    LCNF `;` tiledOuterTwo `;` LCNF,
    LCNF `;` splitK `;` LCNF,
    LCNF `;` prepareFusion `;` LCNF,
    LCNF `;` fusedReduceMapAgain `;` LCNF,
    LCNF `;` moveOuterKLoopOnce `;` LCNF,
    LCNF `;` moveInnerKLoopOnce `;` LCNF `;` LCNF
  )

}

// -- todo -- change this
// only a wip solution
// implementation of problems
// used to calculate gold in executor
// move this to object rise exploration
object Programs {
  // input size
  val N = 1 << 9

  val mm = infer(
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        map(fun(ak =>
          map(fun(bk =>
            (reduce(add)(l(0.0f)) o
              map(fun(x => fst(x) * snd(x)))) $
              zip(ak, bk))) $ transpose(b) )) $ a))
  )

  val dot = infer(
    fun(ArrayType(N, f32))(a =>
      fun(ArrayType(N, f32))(b =>
        reduce(add)(l(0.0f)) o map(fun(x => fst(x) * snd(x))) $ zip(a,b)))
  )


  val scal = infer(fun(ArrayType(N, f32))(input =>
    fun(f32)(alpha =>
      map(fun(x => alpha * x)) $ input))
  )

}

object AlanParser {

  // classes to parse
  case class ParseExecutor(name:String, iterations:Int, threshold: Double)
  case class ParseMetaheuristic(heuristic: String, depth: Int, iteration: Int)
  case class ParseExploration(name: String, strategies: String, input:String, output:String, inputSize:Int, metaheuristic: Seq[ParseMetaheuristic], executor:ParseExecutor)

  // implicit readings
  implicit val executorRead: Reads[ParseExecutor] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "iterations").read[Int] and
        (JsPath \ "threshold").read[Double]
    )(ParseExecutor.apply _)

  implicit val metaheuristicReads: Reads[ParseMetaheuristic] = (
    (JsPath \ "heuristic").read[String] and
      (JsPath \ "depth").read[Int] and
      (JsPath \ "iterations").read[Int]
    )(ParseMetaheuristic.apply _)

  implicit val explorationReads: Reads[ParseExploration] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "strategies").read[String] and
      (JsPath \ "input").read[String] and
      (JsPath \ "output").read[String] and
      (JsPath \ "inputSize").read[Int] and
      (JsPath \ "metaheuristic").read[Seq[ParseMetaheuristic]] and
      (JsPath \ "executor").read[ParseExecutor]
    )(ParseExploration.apply _)

  def parse(filePath: String): Metaheuristic[Rise] = {

    // read in json from path to string
    val jsonString = readFile(filePath)

    // parse string to json object
    val json = Json.parse(jsonString)

    // parse elements from object
    val result = json.validate[ParseExploration]

    // -- todo --check elements -> requirements

    // now traverse result and create elements
    val inputSize = result.get.inputSize
    result.get.input
    val name = result.get.name
    val output = result.get.output
    result.get.strategies


    // maybe print json information

    // stick exploration together

    // read input expression
    // -- todo --  read expression from file
    val solution = Programs.mm
    val lowering = elevate.rise.rules.lowering.lowerToC
    val gold = lowering.apply(solution).get

    // begin with executor
    val executor = result.get.executor.name match {
      case "C" => new CExecutor(lowering, gold, result.get.executor.iterations, inputSize, result.get.executor.threshold)
      case "OpenMP" => new Exception("executor option not yet implemented")
      case "OpenCL" => new Exception("executor option not yet implemented")
      case _ => new Exception("not a supported executor option")
    }

    // check if output path already exists
    var uniqueFilename_full = output + "/" + name
    if(Files.exists(Paths.get(uniqueFilename_full))){
      println("path is there")
      val warningString = "Warning! Clash at " + uniqueFilename_full + ".\n"
      println(warningString + "adding System.currentTimeMillis().")
      uniqueFilename_full = uniqueFilename_full + "_" + System.currentTimeMillis()
    }
    // create output folder
    (s"mkdir ${uniqueFilename_full}" !!)

    // create names
    val nameList = scala.collection.mutable.ListBuffer.empty[String]
    var predecessor = uniqueFilename_full
    result.get.metaheuristic.foreach(elem => {
      predecessor = predecessor + "/" + elem.heuristic
      nameList += predecessor
    })

    // create subfolders
    nameList.foreach(elem => {
      println("elem: " + elem)
      (s"mkdir ${elem}" !!)
    })

    var index = 0

    // root metaheuristic using executor as executor
    val rootChoice = result.get.metaheuristic.reverse.head
    val rootMetaheuristic = new Metaheuristic[Rise](rootChoice.heuristic, getHeuristic(rootChoice.heuristic),
      rootChoice.depth,rootChoice.iteration, executor.asInstanceOf[CExecutor], Strategies.strategies, nameList.reverse.apply(index))
    index = index + 1

    // iterate reverse direction
    var metaheuristic = rootMetaheuristic
    result.get.metaheuristic.reverse.tail.foreach(elem => {
      // new metaheuristic with last one as Runner
      metaheuristic = new Metaheuristic[Rise](elem.heuristic, getHeuristic(elem.heuristic),
        elem.depth,elem.iteration, metaheuristic, Strategies.strategies, nameList.reverse.apply(index))
      index = index + 1
    })

    metaheuristic
  }

  def getHeuristic(name:String):Heuristic[Rise] = {
    val heuristic = name match {
      case "IterativeImprovement" => new IterativeImprovement[Rise]
      case "Random" => new Random[Rise]
      case _ => new Exception("not a supported heuristic option")
    }
    heuristic.asInstanceOf[Heuristic[Rise]]
  }

  def readFile(filePath: String):String = {
    // prepare output string
    var output = """"""
    // open file
    val bufferedSource = Source.fromFile(filePath)
    // read in file
    for (line <- bufferedSource.getLines) {
      output += line
    }

    // close file properly
    bufferedSource.close

    output
  }

}


