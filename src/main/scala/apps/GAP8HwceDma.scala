package apps

import rise.GAP8.DSL.gap8Run
import rise.GAP8.primitives.{allocL1, copyToL1, copyToL2, gap8hwConv3x3}
import rise.core.DSL.Type.TypeConstructors
import rise.core.DSL.Type.`.`
import rise.core.DSL.{ToBeTyped, depFun, fun, letf}
import rise.core.primitives.{add, mapSeq}
import rise.core.types.DataType.{ArrayType, i16, u8}
import rise.core.types.Nat
import rise.elevate.Rise
import shine.GAP8.Module.translateToString

object GAP8HwceDma {
  def main(args: Array[String]): Unit = {
    val size: Nat = 10
    val fixSize: ToBeTyped[Rise] = fun(ArrayType(size, u8) ->: ArrayType(size, u8))(in =>
        gap8Run(8)(
          in |>
            copyToL1 |>
            allocL1 |>
            mapSeq(fun(x => x)) |>
            allocL1 |>
            copyToL2
        )
      )
    println(translateToString(util.gen.gap8.hosted.fromExpr(fixSize)))

    val varSize: ToBeTyped[Rise] = depFun((sz: Nat) =>
      fun(ArrayType(sz, u8) ->: ArrayType(sz, u8))(in =>
        gap8Run(8)(
          in |>
            copyToL1 |>
            allocL1 |>
            mapSeq(fun(x => x)) |>
            allocL1 |>
            copyToL2
        )))
    //println(translateToString(util.gen.gap8.hosted.fromExpr(varSize)))

    val hwceDebug: ToBeTyped[Rise] = fun(
      ArrayType(size, ArrayType(size, u8)) ->:
        ArrayType(3, ArrayType(3, u8)) ->:
        ArrayType(size - 2, ArrayType(size - 2, u8)))((in, filter) =>
      gap8Run(8)(
        gap8hwConv3x3(0)(in, filter)
      )
    )
    println(translateToString(util.gen.gap8.hosted.fromExpr(hwceDebug)))


    val fixSizeDmaHwce: ToBeTyped[Rise] = fun(
      ArrayType(size, ArrayType(size, u8)) ->:
        ArrayType(3, ArrayType(3, u8)) ->:
        ArrayType(size - 2, ArrayType(size - 2, u8)))((in, filter) =>
      gap8Run(8)(
        in |>
          copyToL1 |>
          allocL1 |>
          letf(innerin =>
            filter |>
              copyToL1 |>
              allocL1 |> letf(innerf =>
                gap8hwConv3x3(0)(innerin, innerf) |> allocL1 |> copyToL2
            )
          )
      )
    )
    println(translateToString(util.gen.gap8.hosted.fromExpr(fixSizeDmaHwce)))

    /*val varSize: ToBeTyped[Rise] = depFun((n: Nat) =>
      fun(ArrayType(n, u8) ->: ArrayType(n, u8))(in =>
        gap8Run(8)(
          in |>
            copyToL1 |>
            allocL1 |>
            mapSeq(fun(x => x)) |>
            allocL1 |>
            copyToL2
        )
      )
    )
    println(translateToString(util.gen.gap8.hosted.fromExpr(varSize)))*/

    /*
    *
    val w: Nat = 9
    val h: Nat = 9
    val twoDandHWCE: ToBeTyped[Rise] = {
      fun(ArrayType(w, ArrayType(h, i16)) ->: ArrayType(3, ArrayType(3, i16)) ->: ArrayType((w-2), ArrayType((h-2), i16)))((in, filter) =>
        gap8Run(8)(
          in |>
            copyToL1 |>
            allocL1 |>
            mapSeq(fun(x =>x)) |>
            allocL1 |>
            copyToL2
          //gap8hwConv3x3(0)(in)(filter)
        )
      )
    }
    println(translateToString(util.gen.gap8.hosted.fromExpr(twoDandHWCE)))
    * */
  }
}
