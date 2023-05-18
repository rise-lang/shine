package apps

import rise.GAP8.DSL.gap8Run
import rise.GAP8.primitives.{allocL1, copyToL1, copyToL2, gap8hwConv3x3}
import rise.core.DSL.HighLevelConstructs.{slide2D, zipND}
import rise.core.DSL.Type.TypeConstructors
import rise.core.DSL.Type.`.`
import rise.core.DSL.{ToBeTyped, TypeAnnotationHelper, depFun, foreignFun, fun, let, letf, li16, lu8}
import rise.core.primitives.{add, cast, fst, join, mapSeq, padCst, slide, snd, zip}
import rise.core.types.DataType.{ArrayType, i16, u8}
import rise.core.types.Nat
import rise.elevate.Rise
import rise.openMP.primitives.mapPar
import shine.GAP8.Module.translateToString

// scalastyle: off
object GAP8HwceDma {
  def main(args: Array[String]): Unit = {
    val gapSqrt = foreignFun("gap_sqrt",
      Seq("a_nInput"),
      """
        | {
        |   uint32_t op  = a_nInput;
        |   uint32_t res = 0;
        |
        |   uint32_t one = 1uL << 30;
        |   while (one > op){
        |     one >>= 2;
        |   }
        |   while (one != 0) {
        |     if (op >= res + one){
        |       op = op - (res + one);
        |       res = res +  2 * one;
        |     }
        |     res >>= 1;
        |     one >>= 2;
        |   }
        |   return res;
        | }
        |""".stripMargin,
      i16 ->: i16
    )

    val size: Nat = 4
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
    //println(translateToString(util.gen.gap8.hosted.fromExpr(fixSize)))

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

    val fixSizeDmaHwce: ToBeTyped[Rise] = fun(
      ArrayType(size, ArrayType(size, i16)) ->:
        ArrayType(3, ArrayType(3, i16)) ->:
        ArrayType(size - 2, ArrayType(size - 2, i16)))((in, filter) =>
      gap8Run(8)(
        in |> copyToL1 |> allocL1 |> letf(innerin =>
          filter |> copyToL1 |> allocL1 |> letf(innerf =>
            gap8hwConv3x3(0)(innerin, innerf) |> allocL1 |> copyToL2
          )
        )
      )
    )
    //println(translateToString(util.gen.gap8.hosted("conv").fromExpr(fixSizeDmaHwce)))

    val n: Nat = 320
    val m: Nat = 240
    val numStripes: Nat = 12
    val stripeSize: Nat = n * (m / numStripes)
    val tileSize: Nat = 80 * 80
    val tiledFixSizeDmaHwce: ToBeTyped[Rise] =
      fun(
        ArrayType(n, ArrayType(m, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(n - 2, ArrayType(m - 2, i16)))((pic, hw, vw) =>
        gap8Run(8)(
          hw |> copyToL1 |> allocL1 |> letf(l1hw =>
            vw |> copyToL1 |> allocL1 |> letf(l1vw =>
              pic |> slide(stripeSize)(stripeSize - 1) |>
                mapSeq(fun(stripe =>
                  stripe |> copyToL1 |> allocL1 |> letf(l1stripe =>
                    l1stripe |> mapPar(fun(row => row |> padCst(1)(1)(li16(0)))) |> // mapSeq(fun(x => cast(x) :: i16)) |>
                        letf(l1convstripe =>
                          gap8hwConv3x3(0)(l1convstripe, l1hw) |> letf(hconvres =>
                            gap8hwConv3x3(0)(l1convstripe, l1vw) |> letf(vconvres =>
                              zip(hconvres)(vconvres) |>
                                mapPar(fun((h, v) =>
                                  gapSqrt(add(h * h)(v * v))
                                )) |> allocL1 |> copyToL2
                            )
                          )
                        )
                  )))
              )
            )
        )
      )
    //println(translateToString(util.gen.gap8.hosted("tiledConv").fromExpr(tiledFixSizeDmaHwce)))

    val tiledFixSizeDmaHwce2: ToBeTyped[Rise] =
      fun(
        ArrayType(n, ArrayType(m, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(n - 2, ArrayType(m - 2, i16)))((pic, hw, vw) =>
        gap8Run(8)(
          hw |> copyToL1 |> allocL1 |> letf(l1hw =>
            vw |> copyToL1 |> allocL1 |> letf(l1vw =>
              pic |> slide(80)(80) |>
                mapSeq(fun(tile =>
                  tile |> copyToL1 |> allocL1 |> letf(l1tile =>
                    gap8hwConv3x3(0)(l1tile, l1hw) |> letf(hconvres =>
                      gap8hwConv3x3(0)(l1tile, l1vw) |> letf(vconvres =>
                        zipND(2)(hconvres, vconvres) |> mapPar(fun((h, v) =>
                          gapSqrt(add(h * h)(v * v))
                        )) |> allocL1 |> copyToL2
                      )
                    )
                  )
                ))
            )
          )
        )
      )

    //println(translateToString(util.gen.gap8.hosted("tiledConv").fromExpr(tiledFixSizeDmaHwce2)))

    val simpleNoSlide: ToBeTyped[Rise] =
      fun(
      ArrayType(size, ArrayType(size, i16)) ->:
        ArrayType(3, ArrayType(3, i16)) ->:
        ArrayType(3, ArrayType(3, i16)) ->:
        ArrayType(size - 2, ArrayType(size - 2, i16)))((pic, hw, vw) =>
      gap8Run(8)(
        hw |> copyToL1 |> allocL1 |> letf(l1hw =>
          vw |> copyToL1 |> allocL1 |> letf(l1vw =>
            pic |> copyToL1 |> allocL1 |> letf(l1pic =>
              gap8hwConv3x3(0)(l1pic, l1hw) |> allocL1 |> letf (hconvres => //hconvres |> copyToL2
                gap8hwConv3x3(0)(l1pic, l1vw) |> allocL1 |> letf (vconvres =>
                  zip(hconvres)(vconvres) |> mapPar(fun(x =>
                    add(fst(x))(snd(x))
                  )) |> allocL1 |> copyToL2
                )
              )
            )
          )
        )
      )
    )
    println(translateToString(util.gen.gap8.hosted("doubleconv").fromExpr(simpleNoSlide)))
  }
}
