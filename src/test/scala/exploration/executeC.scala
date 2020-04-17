import rise.core.TypedDSL.{add, fst, fun, l, map, reduce, snd, transpose, zip}

import rise.core.TypedDSL._
import rise.core.types.{ArrayType, f32, infer}

class executeC extends shine.test_util.Tests {

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
//
//  test("gen code vor scal"){
//    val lowering = elevate.rise.rules.lowering.lowerToC
//
//    val gold = lowering.apply(scal).get
//
//    val test = new exploration.runner.CExecutor(lowering, gold, 5, N, 1.5, "")
//
//    val result = test.execute(scal)
//
//    println("result: " + result._2)
//  }
//
//  test("gen code vor dot"){
//    val lowering = elevate.rise.rules.lowering.lowerToC
//
//    val gold = lowering.apply(dot).get
//
//    val test = new exploration.runner.CExecutor(lowering, gold, 5, N, 1.5)
//
//    val result = test.execute(dot)
//
//    println("result: " + result._2)
//  }
//
//  test("gen code for mm"){
//    val lowering = elevate.rise.rules.lowering.lowerToC
//
//    val gold = lowering.apply(mm).get
//
//    val test = new exploration.runner.CExecutor(lowering, gold, 5, N, 1.5)
//
//    val result = test.execute(mm)
//
//    println("result: " + result._2)
//  }

}
