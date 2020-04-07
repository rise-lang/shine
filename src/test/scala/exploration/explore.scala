package exploration

import arithexpr.arithmetic.ArithExpr
import rise.core.TypedDSL.{add, fst, fun, l, map, reduce, snd, transpose, zip}
import rise.core.types.{ArrayType, f32, infer}
import elevate.core.strategies.traversal.{alltd, oncebu, oncetd}
import elevate.heuristic_search.{Heuristic, Metaheuristic, Runner}
import elevate.rise.Rise
import elevate.rise.rules.algorithmic.{blockedReduce, fissionReduceMap, fuseReduceMap}
import elevate.rise.rules.lowering.{mapSeq, reduceSeq}
import elevate.rise.rules.movement.{liftReduce, mapFBeforeSlide}
import elevate.rise.rules.traversal.LiftTraversable
import elevate.rise.strategies.normalForm.{CNF, LCNF, RNF}
import elevate.rise.strategies.tiling.tileNDList
import play.api.libs.json
import play.api.libs.json.{JsDefined, JsError, JsPath, JsResult, JsSuccess, JsUndefined, Reads}
import util._
//import exploration.search.executeC.genExecutableCode



//test to run exploration
class explore extends shine.test_util.Tests {

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

  test("scal exploration"){
    val lowering = elevate.rise.rules.lowering.lowerToC
    val result = riseExploration(scal, lowering, N)
    println("result: " + result)
  }

  test("dot exploration"){
    val lowering = elevate.rise.rules.lowering.lowerToC
    val result = riseExploration(dot, lowering, N)
    println("result: " + result)
  }

  test("mm exploration"){
    val lowering = elevate.rise.rules.lowering.lowerToC
    val result = riseExploration(mm, lowering, N)
    println("result: " + result)
  }

  test("alan parser"){

    val metaheuristic = AlanParser.parse("/home/jo/developement/rise-lang/halde/dot_0.json")
//    val metaheuristic = AlanParser.parse("/home/jo/developement/rise-lang/halde/dot_1.json")
//    val metaheuristic = AlanParser.parse("/home/jo/developement/rise-lang/halde/dot_2.json")

    metaheuristic.execute(mm)
  }




}

