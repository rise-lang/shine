package apps

import arithexpr.arithmetic.NamedVar
import elevate.core.Strategy
import rise.GAP8.DSL.gap8Run
import rise.GAP8.primitives.{allocL1, copy2DOffsetToL1, copyToL1, copyToL2, gap8hwConv3x3, gap8hwConv5x5}
import rise.core.DSL.HighLevelConstructs.{padCst2D, zipND}
import rise.core.DSL.Type.TypeConstructors
import rise.core.DSL.{ToBeTyped, depFun, foreignFun, fun, letf, li16}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, i16, int, u8}
import rise.core.types.Nat
import rise.elevate.Rise
import rise.elevate.strategies.traversal.everywhere
import rise.elevate.strategies.traversal.AtHelper
import rise.elevate.rules.lowering.`map -> mapPar`
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

    //Passes
    val theSmallestSlide: ToBeTyped[Rise] = fun(
      ArrayType(5, i16) ->: ArrayType(3, ArrayType(3, i16))
    )(arr =>
      gap8Run(8)(
        arr |> slide(3)(1) |> mapSeq(mapSeq(fun(elem => add(elem)(li16(1)))))
      )
    )
    //printf(translateToString(util.gen.gap8.hosted("theSmallest").fromExpr(theSmallestSlide)))
    //translateToString(util.gen.gap8.hosted("theSmallest").fromExpr(theSmallestSlide))
    println("======================================================================")

    val inBetweenSlideExample: ToBeTyped[Rise] = fun(
      ArrayType(5, i16) ->: ArrayType(3, ArrayType(3, i16))
    )(arr =>
      gap8Run(8)(
        arr |> slide(3)(1) |> mapSeq(fun(tile =>
          tile |> copyToL1 |> allocL1 |> letf(tstletf =>
            tstletf |> mapSeq(fun(elem => add(elem)(li16(1)))) |> allocL1 |> copyToL2
          )
        ))
      )
    )
    //println(translateToString(util.gen.gap8.hosted("inBetween").fromExpr(inBetweenSlideExample)))
    //translateToString(util.gen.gap8.hosted("inBetween").fromExpr(inBetweenSlideExample))
    println("======================================================================")

    //Does not pass
    val thisMaybe: ToBeTyped[Rise] = fun(
      ArrayType(5, i16) ->: ArrayType(9, i16)
    )(arr =>
      gap8Run(8)(
        arr |> slide(3)(1) |> join |> copyToL1 |> allocL1 |> letf(sth =>
          sth |> mapSeq(fun(elem => add(elem)(li16(1)))) |> allocL1 |> copyToL2
        )
      )
    )
    //println(translateToString(util.gen.gap8.hosted("thisMaybe").fromExpr(thisMaybe)))
    //translateToString(util.gen.gap8.hosted("thisMaybe").fromExpr(thisMaybe))

    //Does not pass either
    val evenSmallerSlideExample: ToBeTyped[Rise] = fun(
      ArrayType(5, i16) ->: ArrayType(3, ArrayType(3, i16))
    )(arr =>
      gap8Run(8)(
        arr |> slide(3)(1) |> mapSeq(fun(tile =>
          tile |> copyToL1 |> allocL1 |> letf(tilel1 =>
            tilel1 |> mapSeq(fun(elem => add(elem)(li16(1)))) |> allocL1 |> copyToL2
          )
        ))
      )
    )
    //println(translateToString(util.gen.gap8.hosted("evenSmallerSlideExample").fromExpr(evenSmallerSlideExample)))
    //translateToString(util.gen.gap8.hosted("evenSmallerSlideExample").fromExpr(evenSmallerSlideExample))

    //No way
    val minSlideExample: ToBeTyped[Rise] = fun(
      ArrayType(5, ArrayType(5, i16)) ->:
        ArrayType(3, ArrayType(5, i16)) ->:
      ArrayType(9, ArrayType(5, i16)))((sth, filter) =>
      gap8Run(8)(
        sth |> slide(3)(1) |> mapSeq(fun(tile =>
          tile |> copyToL1 |> allocL1 |> letf(tilel1 =>
            zipND(2)(tilel1, filter) |> mapPar(mapPar(fun(elems =>
              add(fst(elems))(snd(elems))
            ))) |> allocL1 |> copyToL2
          )
        )) |> join
      )
    )
    //println(translateToString(util.gen.gap8.hosted("minSlide").fromExpr(minSlideExample)))

    val w: Nat = 320
    val h: Nat = 240
    //Padded horizontally
    val tiledFixSizeDmaHwce: ToBeTyped[Rise] =
      fun(
        ArrayType(h, ArrayType(w, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(h - 2, ArrayType(w, i16)))((pic, hw, vw) =>
        gap8Run(8)(
          hw |> copyToL1 |> allocL1 |> letf(l1hw =>
            vw |> copyToL1 |> allocL1 |> letf(l1vw =>
               // pic |> padCst2D(1, 1, 0, 0)(li16(0)) |> slide(16)(14) |>
                // pic: 240.320.i16
                pic |> slide (16)(14) |>
                // 14*16+16 => 240 (n: 16)
                // 17.16.320.i16
                mapSeq(fun(stripe =>
                  // stripe: 16.320.i16
                  stripe |> copy2DOffsetToL1(0)(1) |> allocL1 |> // letf(l1stripe =>

                    // l1stripe |> padCst2D(0, 0, 1, 1)(li16(0)) |>
                      // mapSeq(mapSeq(fun(x => x))) |> allocL1 |>
                      // l1convstripe: 18.322.i16
                    // if offsetH = 0 -> 16.322.i16
                      letf(l1convstripe =>
                        // convresult(s): 16.320.i16
                        // if offsetH =0 -> 14.320.i16
                        gap8hwConv3x3(0)(l1convstripe, l1hw) |> allocL1 |> letf(hconvres =>
                          gap8hwConv3x3(0)(l1convstripe, l1vw) |> allocL1 |> letf(vconvres =>
                            // zipND: 16.320.(i16, i16)
                            zipND(2)(hconvres)(vconvres) |> mapPar(mapPar(fun(elems =>
                              gapSqrt(add(mul(fst(elems))(fst(elems)))(mul(snd(elems))(snd(elems))))
                              //copy back 16.320.i16
                            ))) |> allocL1 |> copyToL2
                          )
                        )
                      // )
                  )
                  //(17*16).320.i16
                  //offsetH -> 0 (17*14).320.i16
                )) |> join
              )
            )
        )
      )
    //println(translateToString(util.gen.gap8.hosted("tiledConv").fromExpr(tiledFixSizeDmaHwce)))

    //Padded vertically
    val tiledFixSizeDmaHwcePaddedVertically: ToBeTyped[Rise] =
      fun(
        ArrayType(h, ArrayType(w, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(h - 2, ArrayType(w - 2, i16)))((pic, hw, vw) =>
        gap8Run(8)(
          hw |> copyToL1 |> allocL1 |> letf(l1hw =>
            vw |> copyToL1 |> allocL1 |> letf(l1vw =>
              // pic: 240.320.i16
              pic |> slide(16)(14) |>
                // 14*16+16 => 240 (n: 16)
                // 17.16.320.i16
                mapSeq(fun(stripe =>
                  // stripe: 16.320.i16
                  stripe |> copy2DOffsetToL1(1)(0) |> allocL1 |> // letf(l1stripe =>
                    // l1convstripe 18.320.i16
                    letf(l1convstripe =>
                      // convresult(s): 16.318.i16
                      gap8hwConv3x3(0)(l1convstripe, l1hw) |> allocL1 |> letf(hconvres =>
                        gap8hwConv3x3(0)(l1convstripe, l1vw) |> allocL1 |> letf(vconvres =>
                          // zipND: 16.318.(i16, i16)
                          zipND(2)(hconvres)(vconvres) |> mapPar(mapPar(fun(elems =>
                            gapSqrt(add(mul(fst(elems))(fst(elems)))(mul(snd(elems))(snd(elems))))
                            //copy back 16.318.i16
                          ))) |> allocL1 |> take(15) |> drop(1) |> copyToL2
                        )
                      )
                    )
                  //(17*16).318.i16
                )) |> join
            )
          )
        )
      )
    //println(translateToString(util.gen.gap8.hosted("tiledConv").fromExpr(tiledFixSizeDmaHwcePaddedVertically)))

    val tiledFixSizeDmaHwceNoPadding: ToBeTyped[Rise] =
      fun(
        ArrayType(h, ArrayType(w, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(3, ArrayType(3, i16)) ->:
          ArrayType(h - 2, ArrayType(w - 2, i16)))((pic, hw, vw) =>
        gap8Run(8)(
          hw |> copyToL1 |> allocL1 |> letf(l1hw =>
            vw |> copyToL1 |> allocL1 |> letf(l1vw =>
              // pic: 240.320.i16
              pic |> slide(16)(14) |>
                // 14*16+16 => 240 (n: 16)
                // 17.16.320.i16
                mapSeq(fun(stripe =>
                  // stripe: 16.320.i16
                  stripe |> copyToL1 |> allocL1 |> // letf(l1stripe =>
                    // l1convstripe 16.320.i16
                    letf(l1convstripe =>
                      // convresult(s): 14.318.i16
                      gap8hwConv3x3(0)(l1convstripe, l1hw) |> allocL1 |> letf(hconvres =>
                        gap8hwConv3x3(0)(l1convstripe, l1vw) |> allocL1 |> letf(vconvres =>
                          // zipND: 14.318.(i16, i16)
                          zipND(2)(hconvres)(vconvres) |> mapPar(mapPar(fun(elems =>
                            gapSqrt(add(mul(fst(elems))(fst(elems)))(mul(snd(elems))(snd(elems))))
                            //copy back 14.318.i16
                          ))) |> allocL1 |> copyToL2
                        )
                      )
                    )
                  //

                )) |> join
            )
          )
        )
      )
    //println(translateToString(util.gen.gap8.hosted("tiledConv").fromExpr(tiledFixSizeDmaHwceNoPadding)))
    val ww: Nat = 320
    val hh: Nat = 244
    val tiledFixSizeDmaHwceNoPadding_Gaussian: ToBeTyped[Rise] =
      fun(
        ArrayType(hh, ArrayType(ww, i16)) ->:
          ArrayType(5, ArrayType(5, i16)) ->:
          ArrayType(hh - 4, ArrayType(ww - 4, i16)))((pic, filter) =>
        gap8Run(8)(
          filter |> copyToL1 |> allocL1 |> letf(l1filter =>
            // pic: 240.320.i16
            pic |> slide(16)(12) |>
              // step * n + size = 240
              // 17.16.320.i16
              mapSeq(fun(stripe =>
                //stripe 16.320.i16
                stripe |> copyToL1 |> allocL1 |>
                  letf(l1stripe =>
                    // convresult (12.316.i16)
                    gap8hwConv5x5(0)(l1stripe, l1filter) |> allocL1 |> copyToL2
                  )
              )) |> join
          )
        )
      )
    println(translateToString(util.gen.gap8.hosted("GaussianBlur").fromExpr(tiledFixSizeDmaHwceNoPadding_Gaussian)))

    /*val strategy: Strategy[Rise] =
      `map -> mapPar` `@` everywhere
    val ex_mapfor: ToBeTyped[Rise] = depFun((n: Nat) =>
      fun(ArrayType(n, int) ->: ArrayType(n, int))(input =>
        input |> map(fun(x => x))
      )
    )
    println(util.gen.openmp.function("ex_mapfor").asStringFromExpr(strategy(ex_mapfor).get))*/

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
              gap8hwConv3x3(0)(l1pic, l1hw) |> allocL1 |> letf (hconvres => // hconvres |> copyToL2
                gap8hwConv3x3(0)(l1pic, l1vw) |> allocL1 |> letf (vconvres =>
                  zipND(2)(hconvres)(vconvres) |> mapPar(mapPar(fun(elems =>
                    add(fst(elems))(snd(elems))
                  ))) |> allocL1 |> copyToL2
                  /*zip(hconvres)(vconvres) |> mapPar(fun(rows =>
                    zip(fst(rows))(snd(rows)) |> mapPar(fun(elems =>
                      add(fst(elems))(snd(elems))
                    ))
                  )) |> allocL1 |> copyToL2*/
                )
              )
            )
          )
        )
      )
    )
    //println(translateToString(util.gen.gap8.hosted("doubleconv").fromExpr(simpleNoSlide)))
  }
}
