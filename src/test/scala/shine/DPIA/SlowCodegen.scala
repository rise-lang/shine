package shine.DPIA

import rise.core._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.DSL.HighLevelConstructs._
import rise.core.primitives._
import rise.core.types._
import util.gen

class SlowCodegen extends test_util.Tests {
  // FIXME: compilation takes ~4mn and generates huge code with too many ifs
  ignore("multiscale interpolation subexpression") {
    val normalize: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
      (4`.`h`.`w`.`f32) ->: (4`.`h`.`w`.`f32)
    )(input =>
      input |> transpose |> map(transpose) |> // H.W.4.
        map(map(fun { p =>
          generate(fun(i => p `@` i / p `@` lidx(3, 4))) :: (4`.`f32)
        })) |> map(transpose) |> transpose
    ))

    val expr: ToBeTyped[Expr] = depFun((h: Nat, w: Nat) => fun(
      (4`.`h`.`w`.`f32) ->: (4`.`h`.`w`.`f32)
    )(input => {
      val alpha = lidx(3, 4)
      generate(fun(i =>
        select(i < alpha)(
          zipND(2)(input `@` i, input `@` alpha) |>
          map(map(fun(p => fst(p) * snd(p))))
        )(input `@` alpha)
      )) |> map(padClamp2D(0, 1)) |>
      map(dropLast(1)) >>
      map(map(dropLast(1))) >>
      normalize(h)(w) >> mapSeq(mapSeq(mapSeq(fun(x => x))))
    }))

    val code = gen.c.function.asStringFromExpr(uniqueNames.enforce(expr.toExpr))
    // last recorded code length: 61 868 345
    // maximum reasonable code length: ~ 20 lines of 100 chars
    assert(code.length < 20*100)
  }
}
