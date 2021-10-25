package shine.GAP8

import elevate.core.Strategy
import rise.GAP8.DSL.{gap8Run, hwce}
import rise.GAP8.primitives.{gap8RunPrimitive, gap8hwConv3x3, gap8hwConv5x5, gap8hwConv7x4, gap8hwConv7x7}
import rise.core.DSL.HighLevelConstructs._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.elevate.Rise
import rise.elevate.rules.algorithmic.gap8hwConvMerge
import rise.elevate.strategies.traversal._
import shine.GAP8

class hwce extends test_util.Tests {

  val exprNoPipes: ToBeTyped[Rise] = {
    fun((6`.`6`.`i16) ->: (3`.`3`.`i16) ->: ((6 - 2)`.`(6 - 2)`.`i16))((in, filter) =>
      gap8Run(8)(
        mapSeq(mapSeq(fun(sub => {
          reduceSeq(add)(li16(0))(map(fun(x => fst(x) * snd(x)))(zip(join(sub))(join(filter))))
        })))(slide2D(3, 1)(in))
      )
    )
  }

  private def checkHwceCall(code: String, filterSize: String) = {
    assert(
      ("HWCE_ProcessOneTile" + filterSize).r.findAllIn(code).length >= 1
    )
  }

  test("Optimization strategy 3x3") {
    val w: Nat = 6
    val h: Nat = 6

    /**
      * TODO: Pad filter with one 0 or do that in data prep step on backend (codegen)
      * HWCE_ProcessOneTile3x3_MultiOut(e1, output, NULL, NULL, e2, 0, n, m, 0x7)
      * */
    val exprOnAcc: ToBeTyped[Rise] = {
      fun((w`.`h`.`i16) ->: (3`.`3`.`i16) ->: ((w - 2)`.`(h - 2)`.`i16))((in, filter) =>
        gap8Run(8)(
          in |>
            slide2D(3, 1) |>
            mapSeq(mapSeq(fun(sub => {
              zip(sub |> join)(filter |> join) |>
                map(fun(x => fst(x) * snd(x))) |>
                reduceSeq(add)(li16(0))
            })))
        )
      )
    }

    val conv: Strategy[Rise] =
      (gap8hwConvMerge `@` everywhere)

    val lowExpr = conv(exprOnAcc).get
    val module = util.gen.gap8.hosted.fromExpr(lowExpr)
    val code = GAP8.Module.translateToString(module)

    checkHwceCall(code, "3x3")
  }

  ignore("Optimization strategy 5x5") {
    val w: Nat = 10
    val h: Nat = 10

    /**
      * HWCE_ProcessOneTile5x5(e1, output, e2, 0, n, m)
      * */
    val exprOnAcc: ToBeTyped[Rise] = {
      fun((w`.`h`.`i16) ->: (5`.`5`.`i16) ->: ((w - 4)`.`(h - 4)`.`i16))((in, filter) =>
        gap8Run(8)(
          in |>
            slide2D(5, 1) |>
            mapSeq(mapSeq(fun(sub => {
              zip(sub |> join)(filter |> join) |>
                map(fun(x => fst(x) * snd(x))) |>
                reduceSeq(add)(li16(0))
            })))
        )
      )
    }

    val conv: Strategy[Rise] =
      (gap8hwConvMerge `@` everywhere)

    val lowExpr = conv(exprOnAcc).get
    val module = util.gen.gap8.hosted.fromExpr(lowExpr)
    val code = GAP8.Module.translateToString(module)

    checkHwceCall(code, "5x5")
  }

  ignore("Optimization strategy 7x7") {
    val w: Nat = 10
    val h: Nat = 10

    /**
      * HWCE_ProcessOneTile7x7(e1, output, e2, 0, n, m)
      * */
    val exprOnAcc: ToBeTyped[Rise] = {
      fun((w`.`h`.`i16) ->: (7`.`7`.`i16) ->: ((w - 6)`.`(h - 6)`.`i16))((in, filter) =>
        gap8Run(8)(
          in |>
            slide2D(7, 1) |>
            mapSeq(mapSeq(fun(sub => {
              zip(sub |> join)(filter |> join) |>
                map(fun(x => fst(x) * snd(x))) |>
                reduceSeq(add)(li16(0))
            })))
        )
      )
    }

    val conv: Strategy[Rise] =
      (gap8hwConvMerge `@` everywhere)

    val lowExpr = conv(exprOnAcc).get
    val module = util.gen.gap8.hosted.fromExpr(lowExpr)
    val code = GAP8.Module.translateToString(module)

    checkHwceCall(code, "7x7")
  }

  test("Direct use of gap8hwConv3x3") {
    val w: Nat = 9
    val h: Nat = 9

    /**
      * HWCE_ProcessOneTile3x3_MultiOut(e1, output, NULL, NULL, e2, 0, n, m, 0x7)
      * */
    val expr: ToBeTyped[Rise] = {
      fun((w`.`h`.`i16) ->: (3`.`3`.`i16) ->: ((w - 2)`.`(h - 2)`.`i16))((in, filter) =>
        gap8Run(8)(
          gap8hwConv3x3(0)(in)(filter)
        )
      )
    }

    val hostedModule = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(hostedModule)

    checkHwceCall(code, "3x3")
    //println(code)
  }

  ignore("Direct use of gap8hwConv5x5") {
    val w: Nat = 10
    val h: Nat = 10

    /**
      * HWCE_ProcessOneTile5x5(e1, output, e2, 0, n, m)
      * */
    val expr: ToBeTyped[Rise] = {
      fun((w`.`h`.`i16) ->: (5`.`5`.`i16) ->: ((w - 4)`.`(h - 4)`.`i16))((in, filter) =>
        gap8Run(8)(
          gap8hwConv5x5(0)(in)(filter)
        )
      )
    }

    val hostedModule = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(hostedModule)

    checkHwceCall(code, "5x5")
    //println(code)
  }

  ignore("Direct use of gap8hwConv7x7") {
    val w: Nat = 10
    val h: Nat = 10

    /**
      * HWCE_ProcessOneTile7x7(e1, output, e2, 0, n, m)
      * */
    val expr: ToBeTyped[Rise] = {
      fun((w`.`h`.`i16) ->: (7`.`7`.`i16) ->: ((w - 6)`.`(h - 6)`.`i16))((in, filter) =>
        gap8Run(8)(
          gap8hwConv7x7(0)(in)(filter)
        )
      )
    }

    val hostedModule = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(hostedModule)

    checkHwceCall(code, "7x7")
    //println(code)
  }

  ignore("Direct use of gap8hwConv7x4") {
    val w: Nat = 10
    val h: Nat = 10

    /**
      * HWCE_ProcessOneTile7x4(e1, output, e2, 0, n, m)
      * */
    val expr: ToBeTyped[Rise] = {
      fun((w`.`h`.`i16) ->: (7`.`4`.`i16) ->: ((w - 6)`.`(h - 3)`.`i16))((in, filter) =>
        gap8Run(8)(
          gap8hwConv7x4(0)(in)(filter)
        )
      )
    }

    val hostedModule = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(hostedModule)

    checkHwceCall(code, "7x4")
    //println(code)
  }
}
