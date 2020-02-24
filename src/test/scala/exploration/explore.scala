package exploration

import rise.core.TypedDSL.{add, fst, fun, l, map, reduce, snd, transpose, zip}
import rise.core.types.{ArrayType, f32, infer}
import elevate.core.strategies.traversal.{alltd}
import elevate.rise.rules.lowering.{mapSeq, reduceSeq}
import elevate.rise.rules.traversal.LiftTraversable
import elevate.rise.strategies.normalForm.CNF



//test to run exploration
class explore extends shine.test_util.Tests {

  val N = 1024
  val mm = infer(
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        map(fun(ak =>
          map(fun(bk =>
            (reduce(add)(l(0.0f)) o
              map(fun(x => fst(x) * snd(x)))) $
              zip(ak, bk))) $ transpose(b) )) $ a))
  )

  test("test exploration"){
    val lowering = CNF `;` alltd(reduceSeq) `;` alltd(mapSeq)

    val result = riseExploration(mm, lowering)

    println("result: " + result)

  }


}
