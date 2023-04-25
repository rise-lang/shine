package apps

import rise.GAP8.DSL.gap8Run
import rise.GAP8.primitives.{copyToL1, copyToL2}
import rise.core.DSL.Type.TypeConstructors
import rise.core.DSL.{ToBeTyped, fun}
import rise.core.primitives.mapSeq
import rise.core.types.DataType.{ArrayType, u8}
import rise.elevate.Rise
import shine.GAP8.Module.translateToString

object GAP8HwceDma {
  def main(args: Array[String]): Unit = {
    val expr: ToBeTyped[Rise] = fun(ArrayType(10, u8) ->: ArrayType(10, u8))(in =>
      gap8Run(8)(
        in |>
          mapSeq(fun(x => x)) |>
          copyToL1 |>
          mapSeq(fun(x => x)) |>
          copyToL2
      )
    )

    println(translateToString(util.gen.gap8.hosted.fromExpr(expr)))
  }
}
