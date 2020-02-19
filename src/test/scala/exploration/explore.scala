package exploration

import apps.convolution
import apps.mm.dotSeq
import elevate.core.Strategy
import elevate.core.strategies.basic.{`try`, repeat, repeatNTimes}
import elevate.core.strategies.traversal.{alltd, bottomup, downup, downup2, oncebu, oncetd, one, somebu, topdown, tryAll}
import elevate.rise.rules.lowering
import elevate.rise.strategies.normalForm.{BENF, CNF, LCNF, RNF}
import shine.DPIA.Types.{ExpType, read, write}
import rise.core.DSL.{fst, fun, l, mapSeq, nFun, reduceSeq, snd, zip, _}
//import rise.core.TypedDSL.{fst, fun, l, mapSeq, nFun, reduceSeq, snd, zip, map, reduce}
import rise.core.HighLevelConstructs.reorderWithStride
import rise.core.dotPrinter
import util.gen
import rise.core._
import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import util.gen
import arithexpr.arithmetic.?
import elevate.core.Strategy
import elevate.heuristic_search.ProblemConstraints
import elevate.rise.Rise
import elevate.core.strategies.basic.{`try`, normalize, repeatNTimes}
import elevate.core.strategies.traversal.{oncetd, one}
import elevate.rise.rules.traversal.function
import elevate.fsmooth.rules.buildGet
import elevate.rise.rules.algorithmic._
import elevate.rise.meta.traversal._
import elevate.rise.rules._
import elevate.rise.rules.lowering.mapSeq
import rise.core.ShowRise
import elevate.rise.rules.traversal.LiftTraversable
import rise.core.types.{ArrayType, NatIdentifier, f32}


//test to run exploration
class explore extends shine.test_util.Tests {
//
//  //simple example expression
//  val s = fun(f => fun(g => map(f) >> map(g)))
//
//  private def xsT(N: NatIdentifier)
//
//  = ArrayType(N, f32)
//
//  private def ysT(N: NatIdentifier)
//
//  = ArrayType(N, f32)
//
//  private val mulT =
//    fun(x => fst(x) * snd(x))
//  private val add =
//    fun(a => fun(x => a + x))
//
//  private val simpleDotProduct =
//    nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
//      zip(xs)(ys) |> map(mulT) |> reduce(add)(l(0.0f))
//    )))
//
  private val simpleScal =
    nFun(n => fun(n `.` f32)(input => fun(f32)(alpha =>
      input |> map(fun(x => alpha * x)))
    ))

  var scal = infer(simpleScal)
//  val scal = simpleScal
//  val dot = infer(simpleDotProduct)
//  val dot = simpleDotProduct


//  val simple_mm: Expr = nFun((n, m, o) => fun(
//    (o`.`n`.`f32) ->: (o`.`m`.`f32) ->: (n`.`m`.`f32)
//  )((at, b) =>
//    transpose(at) |> map(fun(aRow =>
//      transpose(b) |> map(fun(bCol =>
//        simpleDotProduct(aRow)(bCol)
//      ))
//    ))
//  ))
//
//  val mm = simple_mm
//
//  val tmp0  = convolution.blurXTiled2D
//  val tmp = infer(tmp0)

//  // we can use implicit type parameters and type annotations to specify the function type of mult
//  val mult = implDT(dt => fun(x => x._1 * x._2) :: ((dt x dt) ->: dt))
//  val add = fun(x => fun(y => x + y))
//  val scal = implN(n =>
//    fun(xs => fun(a =>
//      map(fun(x => a * x), xs)
//    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
//  )
//  val dot = fun(xs => fun(ys =>
//    zip(xs, ys) |> map(mult) |> reduceSeq(add, l(0.0f))
//  ))
//
//  val high_level = nFun((n, m) => fun(
//    (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
//      (m`.`f32)
//  )((mat, xs, ys, alpha, beta) =>
//    zip(map(fun(row => alpha * dot(row, xs)), mat), scal(ys, beta)) |>
//      map(fun(x => x._1 + x._2))
//  ))


//  val mm = infer(high_level)

//  test("mm exploration"){
//    println("start mm exploration")
//    riseExploration.main(mm)
//  }

  test("scal exploration"){
    println("start scal exploration")
    riseExploration.main(scal)
  }

//  test("dot exploration"){
//    println("start dot exploration")
//    riseExploration.main(dot)
//  }



  test("test normalise"){
//    val strategy = normalize(splitJoin(8):)
//    val strategy = `try`(splitJoin(8))
//    val result = strategy.apply(scal)

  }

  test("test different forms"){
    val expression = scal
    val benf = BENF.apply(expression)
    val lcnf = LCNF.apply(expression)
    val rnf = RNF.apply(expression)
    val cnf = CNF.apply(expression)

    println("BENF: " + benf)
    println("LCNF: " + lcnf)
    println("RNF: " + rnf)
    println("CNF: " + cnf)

  }

  test("test bottomup and topdown"){
    println("scal: " + scal)
    val traversal0 = oncetd
    val traversal1 = topdown
    val traversal2 = bottomup
    val traversal3 = tryAll
    val traversal4 = repeat
    val traversal5 = alltd
    val traversal6 = downup
    val traversal7 = downup2
    val traversal8 = oncebu
    val traversal9 = somebu
    val strategy = splitJoin(8)

//    scal = mm
//    scal = BENF.apply(scal)
//    scal = LCNF.apply(scal)
//    scal = RNF.apply(scal)
//    scal = CNF.apply(scal)


    val result0 = traversal0(strategy).apply(scal)
    println("result0: " + result0)

    val result1 = traversal1(strategy).apply(scal)
    println("result1: " + result1)

    val result2 = traversal2(strategy).apply(scal)
    println("result2: " + result2)

    val result3 = traversal3(strategy).apply(scal)
    println("result3: " + result3)

    val result4 = traversal4(strategy).apply(scal)
    println("result4: " + result4)

    val result5 = traversal5(strategy).apply(scal)
    println("result5: " + result5)

    val result6 = traversal6(strategy).apply(scal)
    println("result6: " + result6)

    val result7 = traversal7(strategy, strategy).apply(scal)
    println("result7: " + result7)

    val result8 = traversal8(strategy).apply(scal)
    println("result8: " + result8)

    val result9 = traversal9(strategy).apply(scal)
    println("result9: " + result9)

  }

  test("test some simple strategies") {
    //add core here
    //    val test = identity(solution)
    val strategy = `try`(liftId)
    val strategy2 = (liftId `;` liftId)
    val strategy3 = one(splitJoin(8))
    val strategy4 = oncetd(splitJoin(8))
    val strategy5 = oncetd(mapFusion)
//    val strategy6 = topDown(splitJoin(8))
//    val strategy7 = applyNTimes(splitJoin(8))
    val lower: Strategy[Rise] = LCNF `;` CNF `;`
      repeatNTimes(3, oncetd(lowering.mapSeq))
    val lower2 = oncetd(lowering.mapSeq)
//    val lower3 = normaliz(lowering.mapSeq)

    val rewrite = RNF
    val codegen = CNF

    val result1 = strategy.apply(scal)
    println("result1: " + result1)
    val result2 = strategy2.apply(scal)
    println("result2: " + result2)
    val result3 = strategy3.apply(scal)
    println("result3: " + result3)
    val result4 = strategy4.apply(scal)
    println("result4: " + result4)
    val result5 = strategy5.apply(result4)
    println("result5: " + result5)
    val lowered = lower.apply(scal)
    println("lowered: " + lowered)
    val lowered2 = lower2.apply(scal)
    println("lowered2: " + lowered2)
    val lowered3 = lower2.apply(result4)
    println("lowered3: " + lowered3)

    val rnf = rewrite.apply(scal)
    println("rewrite: "  + rnf )

    val cnf = codegen.apply(scal)
    println("codegen: " + cnf)
    val cnf2 = codegen.apply(result4)
    println("codegen: " + cnf2)

  }



}
