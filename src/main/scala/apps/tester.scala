package apps

import elevate.core._
import rise.GAP8.DSL.gap8Run
import rise.GAP8.primitives.{copyToL1, copyToL2, gap8hwConv3x3}
import rise.core.DSL.HighLevelConstructs.{slide2D, zipND}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{mapSeq, reduceSeq, let => _, _}
import rise.core.types.DataType.{ArrayType, i16, int, u32, u8}
import rise.core.types._
import rise.elevate._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.strategies.predicate.isPrimitive
import rise.elevate.strategies.traversal._
import rise.openMP.primitives.mapPar
import shine.GAP8.Module.translateToString

// scalastyle:off
object tester {
  def main(args: Array[String]) = {

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
      u32 ->: u32
    )

    val h: Nat = 240
    val w: Nat = 320


    //(h`.`w`.`u8) ->: (3`.`3`.`int) ->: (3`.`3`.`int) ->: (h`.`w`.`u8
    val sobelWithoutPad: ToBeTyped[Rise] =
      fun(
        ArrayType(h, ArrayType(w, u8)) ->:
          ArrayType(3, ArrayType(3, u8)) ->:
          ArrayType(3, ArrayType(3, u8)) ->:
          ArrayType(h - 2, ArrayType(w - 2, u8))
      )((pic, hf, vf) =>
        gap8Run(8)(
        pic |>
          slide2D(sz = 3, st = 1) |>
          mapSeq(mapSeq(fun(submat =>
            zip(submat |> join)(hf |> join) |> map(fun(x => (cast(fst(x)) :: u32) * cast(snd(x)) :: u32)) |> reduceSeq(add)(cast(l(0)) :: u32) |> letf(h =>
              zip(submat |> join)(vf |> join) |> map(fun(x => (cast(fst(x)) :: u32) * cast(snd(x)) :: u32)) |> reduceSeq(add)(cast(l(0)) :: u32) |> letf(v =>
                cast(gapSqrt(h * h + v * v)) :: u8
              )
            )
          ))))
      )

    println(translateToString(util.gen.gap8.hosted("SobelWOutPad").fromExpr(sobelWithoutPad)))

    //2D Zip
    //Merge mapPar
    //1D slide
    /*val againExpr: ToBeTyped[Rise] = depFun((n: Nat, m: Nat) =>
      fun((n `.` m `.` u8) ->: (3 `.` 3 `.` i16) ->: (3 `.` 3 `.` i16) ->: ((n - 2) `.` (m - 2) `.` i16))((pic, h_w, v_w) =>
        gap8Run(8)(
          pic |>
            slide(n)(n - 1) |>
            mapSeq(fun(
              stripe => {
                toL1(stripe) |>
                  mapPar(
                    fun(
                      row => {
                        row |> padCst(1)(1)(cast(l(0)) :: u8) |> mapSeq(fun(x => cast(x) :: i16)) |>
                          letf(ImageL1_Conv =>
                            gap8hwConv3x3(0)(ImageL1_Conv)(h_w) |> letf(hs =>
                              gap8hwConv3x3(0)(ImageL1_Conv)(v_w)(0) |> letf(vs =>
                                zipND(2)(hs, vs) |>
                                  mapPar(mapPar(fun(
                                      (h, v) => gapSqrt(h * h + v * v)
                                  ))) |>
                                  toL2
                              )
                            )
                          )
                      }
                    )
                  )
              }
            )
            )
        )
      )
    )*/

    //2D Zip
    //Merge mapPar
    //1D slide
    /*val againAgainExpr: ToBeTyped[Rise] = depFun((n: Nat, m: Nat) =>
      fun(((n * m) `.` u8) ->: (10 `.` i16) ->: (10 `.` i16) ->: (((n - 2) * (m - 2)) `.` i16))((pic, h_w, v_w) =>
        gap8Run(8)(
          pic |>
            slide(n)(n - 1) |>
            mapSeq(fun(
              stripe => {
                toL1(stripe) |>
                  mapPar(
                    fun(
                      row => {
                        row |> padCst(1)(1)(cast(l(0)) :: u8) |> mapSeq(fun(x => cast(x) :: i16)) |>
                          letf(ImageL1_Conv =>
                            gap8hwConv3x3(0)(ImageL1_Conv)(h_w) |> letf(hs =>
                              gap8hwConv3x3(0)(ImageL1_Conv)(v_w) |> letf(vs =>

                                zip(hs)(vs) |>
                                  mapPar(fun(
                                    (h, v) => gapSqrt(h * h + v * v)
                                  ))


                                //zipND(2)(hs, vs) |>
                                //  mapPar(mapPar(fun(
                                //    (h, v) => apps.SobelFilter.gapSqrt(h * h + v * v)
                                //  )))

                              )
                            )
                          )
                      }
                    )
                  )
              }
            )
            )
        )
      )
    )*/

    val testL1asdf: ToBeTyped[Rise] = depFun((n: Nat, m: Nat) =>
      fun((n `.` m `.` u8) ->: (n `.` m `.` u8))(in =>
        gap8Run(8)(
          in |>
            copyToL1 |>
            mapSeq(mapSeq(fun(x =>
              x * lu8(1)
            ))) |>
            copyToL2
        )
      )
    )

    val testL1: ToBeTyped[Rise] = depFun((n: Nat, m: Nat) =>
      fun((n`.`m`.`u8) ->: (n`.`m`.`u8))(in =>
        //gap8Run(8)(
          in |>
            //mapSeq(mapSeq(id)) |>
            //This determines how the memory will be allocated before
            //toL1 |>

            mapSeq(mapSeq(fun(x =>
              x * lu8(1)
            )))
        //)
      )
    )

    //val hostedModule = util.gen.gap8.hosted.fromExpr(testL1)
    //val code = shine.GAP8.Module.translateToString(hostedModule)

    //println(code)

    val testSomethingAgain: ToBeTyped[Rise] = depFun((n: Nat) =>
      fun((n`.`u8) ->: (n`.`u8))(in =>
        in |>
          copyToL1 |>
          mapSeq(fun(x => x))
      )
    )

    val testwhatever: ToBeTyped[Rise] = depFun((n: Nat, m: Nat) =>
      fun(
        (n `.` m `.` i16) ->:
          (3 `.` 3 `.` i16) ->:
          (3 `.` 3 `.` i16) ->:
          ((n) `.` (m) `.` i16)
      )((in, f1, f2) =>
      gap8Run(8)(
        in |>
          copyToL1 |>
          mapSeq(fun(x => x))
      )
    ))


    val testL2: ToBeTyped[Rise] = fun(ArrayType(10, u8) ->: ArrayType(10, u8))(in =>
      gap8Run(8)(
        in |>
          mapSeq(fun(x => x)) |>
          copyToL1 |>
          mapSeq(fun(x => x))// |>
          //copyToL2
          //mapSeq(fun(x => x))
      )
    )
    //println(translateToString(util.gen.gap8.hosted.fromExpr(testL2)))

    //println(util.gen.c.function("asdf").asStringFromExpr(testL2))

    /*val newExpr: ToBeTyped[Rise] = depFun((n: Nat, m: Nat) =>
      fun((n `.` m `.` u8) ->: (3 `.` 3 `.` int) ->: (3 `.` 3 `.` int) ->: (n `.` m `.` u8))((pic, h_w, v_w) =>
        gap8Run(8)(
          pic |>
            slide(sz = stripe_size, st = stripe_size - 1) |> // create stripes
            mapSeq(stripe => // iterate over stripes
              toL1(stripe) |> // copy each stripe to L1
                mapPar(row => // pad and convert eafh stripe
                  row |> padCst(1) |> mapSeq(x => cast(x) :: short_int)) |>
                letf(imageL1conv =>
                  hwce3x3(imageL1conv, h_w) |> letf(hs: 2D => // perform horizontal convolution
                    hwce3x3(imageL1conv, h_v) |> letf(vs: 2D => // perform vertical convolution
                    zip2D(hs, vs) |>
                    mapPar(mapPar((h, v) => sqrt(h * h, v * v))) |> // combine horizontal and vertical convolutions
                    toL2 // copy back to L2
                )
            )
        )
      )
    )
    )
    )*/

    val expr: ToBeTyped[Rise] = depFun((n: Nat, m: Nat, o: Nat) =>
      fun((n`.`o`.`u32) ->: (o`.`m`.`u32) ->: (n`.`m`.`u32))((a, b) =>
        //gap8Run(8)(
          a |> mapSeq(fun(rowa =>
            b |> transpose |> mapSeq(fun(colb =>
              zip(rowa)(colb) |> map(fun(x => fst(x) * snd(x))) |> reduceSeq(add)(cast(l(0)) :: u32)
            ))
          ))
        //)
      )
    )

    val expr123: ToBeTyped[Rise] = depFun((n: Nat, m: Nat, o: Nat) =>
      fun(ArrayType(n, ArrayType(m, u32)) ->: (o `.` m `.` u32) ->: (n `.` m `.` u32))((a, b) =>
        //gap8Run(8)(
        a |> mapSeq(fun(rowa =>
          b |> transpose |> mapSeq(fun(colb =>
            zip(rowa)(colb) |> map(fun(x => fst(x) * snd(x))) |> reduceSeq(add)(cast(l(0)) :: u32)
          ))
        ))
        //)
      )
    )

    //println(util.gen.c.function("matmul").asStringFromExpr(expr))

    //val code = util.gen.openmp.function("matmul").asStringFromExpr(expr)



    /*def weights2d(ws: Seq[Seq[Int]]): ToBeTyped[Expr] =
      larr(ws.map(r => ArrayData(r.map(x => IntData(x)))))

    val sobelXWeights2d: ToBeTyped[Expr] = weights2d(Seq(
      Seq(-1, 0, +1),
      Seq(-2, 0, +2),
      Seq(-1, 0, +1)
    ))*/

    /*val matAddFixed: ToBeTyped[Expr] =
      fun(ArrayType(3, ArrayType(3, int)) ->: ArrayType(3, ArrayType(3, int)) ->: ArrayType(3, ArrayType(3, int)))((x, y) =>
        //gap8Run(8)(
          zip(x)(y) |>
            map(fun(pair => zip(fst(pair))(snd(pair)))) |>
            mapSeq(mapSeq(fun(pair => fst(pair) + snd(pair))))
        //)
      )(sobelXWeights2d)*/

    val matAdd: ToBeTyped[Expr] = depFun((n: Nat, m: Nat) =>
      fun(ArrayType(n, ArrayType(m, u32)) ->: ArrayType(n, ArrayType(m, u32)) ->: ArrayType(n, ArrayType(m, u32)))((x, y) =>
        gap8Run(8)(
          zip(x)(y) |>
            map(fun(pair => zip(fst(pair))(snd(pair)))) |>
            mapPar(mapPar(fun(pair => fst(pair) + snd(pair))))
        )
      )
    )

    val conv: Strategy[Rise] =
      (gap8hwConvMerge `@` everywhere)

    //println(shine.GAP8.Module.translateToString(util.gen.gap8.hosted.fromExpr(conv(expr).get)))
    //println("MatAdd")
    //println(shine.GAP8.Module.translateToString(util.gen.gap8.hosted("matadd").fromExpr(matAddFixed)))
    //println(util.gen.c.function("matadd").asStringFromExpr(matAddFixed))

    /*val simple: Strategy[Rise] =
        (`map -> mapSeq` `@` everywhere)*/


    val optimizationStrategy: Strategy[Rise] =
      (`map -> mapPar`       `@` outermost(isPrimitive(map)))  `;`
        (`map -> mapSeq`       `@` outermost(isPrimitive(map)))  `;`
        (`reduce -> reduceSeq` `@` everywhere)

/*
    val anotherOptimizationStrategy: Strategy[Rise] =
      (`map >> reduce -> reduce` `@` everywhere) `;`
        (`map -> mapSeq`           `@` everywhere) `;`
        (`reduce -> reduceSeq`     `@` everywhere)

    val myStrategy: Strategy[Rise] =
      (`map >> reduce -> reduce` `@` everywhere) `;`
        (`map -> mapPar` `@` outermost(isPrimitive(map))) `;`
        (`map -> mapSeq` `@` everywhere)

    val yetAnotherOptimizationStrategy: Strategy[Rise] =
      innermost(isAppliedMap)(
        `map(f) -> asVector >> map(f_vec) >> asScalar`(4) `;`
          (`map -> mapSeq` `@` innermost(isPrimitive(map))) `;`
          storeTempAsVectors
      ) `;`
        optimizationStrategy*/

    //println(highLevelProgram)
    //println(optimizationStrategy(highLevelProgram).get)
    //println(myStrategy(highLevelProgram).get)

    //println(util.gen.openmp.function("asdf").asStringFromExpr(optimizationStrategy(expr).get))

    //println(expr)
    //println(optimizationStrategy(expr).get)

    //println(util.gen.openmp.function("asdf").asStringFromExpr(myStrategy(highLevelProgram).get))

    //println(shine.GAP8.Module.translateToString(util.gen.gap8.hosted.fromExpr(myStrategy(highLevelProgram).get)))
    //println(shine.GAP8.Module.translateToString(util.gen.gap8.hosted.fromExpr(highLevelProgram)))

    //println(shine.GAP8.Module.translateToString(util.gen.gap8.hosted.fromExpr(expr)))

    //val sth = u32 ->: i16 ->: u8
    //println(sth)
  }
}
