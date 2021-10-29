package shine.GAP8

import rise.GAP8.DSL.gap8Run
import rise.core.DSL.HighLevelConstructs._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.elevate.Rise
import rise.openMP.primitives.mapPar
import shine.GAP8

// scalastyle:off
class codegen extends test_util.Tests {

  private def findParamsStruct(typeName: String, count: Int, code: String) = {
    "struct\\s*cluster_params\\s*\\{[^}]*}"
      .r
      .findFirstIn(code)
      .getOrElse("")
      .toSeq
      .sliding(typeName.length + 1)
      .count(wrapped => wrapped.toString().equalsIgnoreCase(typeName + " "))
      .shouldBe(count)
  }

  private def findDeviceBufferSync(count: Int, code: String) = {
    "deviceBufferSync"
      .r
      .findAllIn(code)
      .length
      .shouldBe(count)
  }

  private def checkCoreNumber(expected: Int, code: String) = {
    "launchKernel\\(.*\\)"
      .r
      .findFirstIn(code)
      .getOrElse("")
      .split(",")(2)
      .trim
      .toInt
      .shouldBe(expected)
  }

  test("Add one on accelerator") {
    val n: Nat = 16
    val expr: ToBeTyped[Rise] =
      fun((n`.`i32) ->: (n`.`i32))(in =>
        gap8Run(4)(mapSeq(add(li32(1)))(in))
      )

    val hostedModule = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(hostedModule)
    
    findDeviceBufferSync(2, code)
    checkCoreNumber(4, code)
    findParamsStruct("int32_t*", 2, code)
  }

  test("Variable size") {
    val expr: ToBeTyped[Rise] = depFun((n: Nat) => fun((n`.`i32) ->: (n`.`i32))(in =>
      gap8Run(8)(mapSeq(add(li32(1)))(in))
    ))

    val hostedModule = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(hostedModule)

    findDeviceBufferSync(2, code)
    checkCoreNumber(8, code)
    findParamsStruct("int32_t*", 2, code)
    findParamsStruct("int", 1, code)
  }

  test("Matmul") {
    val expr: ToBeTyped[Expr] = depFun((n: Nat, m: Nat, o: Nat) =>
      fun((n`.`o`.`u32) ->: (o`.`m`.`u32) ->: (n`.`m`.`u32))((a, b) =>
        gap8Run(8)(
          a |> mapPar(fun(rowa =>
            b |> transpose |> mapPar(fun(colb =>
              zip(rowa)(colb) |> map(fun(x => fst(x) * snd(x))) |> reduceSeq(add)(cast(l(0)) :: u32)
            ))
          ))
        )
      )
    )

    val hostedModule = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(hostedModule)

    findDeviceBufferSync(3, code)
    checkCoreNumber(8, code)
    findParamsStruct("uint32_t*", 3, code)
    findParamsStruct("int", 3, code)
    //println(code)
  }

  test("Sobel filter on GAP8") {
    val expr: ToBeTyped[Rise] = depFun((n: Nat, m: Nat) =>
      fun((n`.`m`.`u8) ->: (3`.`3`.`int) ->: (3`.`3`.`int) ->: (n`.`m`.`u8))((pic, h_w, v_w) =>
        gap8Run(8)(
          pic |>
            padCst2D(1, 1)(cast(l(0)) :: u8) |>
            slide2D(sz = 3, st = 1) |>
            mapPar(mapPar(fun(submat => {
              zip(submat |> join)(h_w |> join) |> map(fun(x => (cast(fst(x)) :: u32) * cast(snd(x)) :: u32)) |> reduceSeqUnroll(add)(cast(l(0)) :: u32) |> letf(h =>
                zip(submat |> join)(v_w |> join) |> map(fun(x => (cast(fst(x)) :: u32) * cast(snd(x)) :: u32)) |> reduceSeqUnroll(add)(cast(l(0)) :: u32) |> letf(v =>
                  cast(apps.SobelFilter.gapSqrt(h * h + v * v)) :: u8
                )
              )
            }
            )))
        )
      )
    )

    val hostedModule = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(hostedModule)


    findDeviceBufferSync(4, code)
    checkCoreNumber(8, code)
    findParamsStruct("uint8_t*", 2, code)
    findParamsStruct("int", 2, code)
    findParamsStruct("int*", 2, code)
    //println(code)
  }

  test("KMeans on GAP8") {
    val testF = foreignFun("test",
      Seq("dist", "tuple"),
      """{
        | uint32_t min_dist = tuple._fst;
        | uint32_t i = tuple._snd._fst;
        | uint32_t index = tuple._snd._snd;
        | if (dist < min_dist) {
        |   return (struct Record_uint32_t__uint32_t_uint32_t_){ dist, { i + 1 , i } };
        | } else {
        |   return (struct Record_uint32_t__uint32_t_uint32_t_){ min_dist, { i + 1, index } };
        | }
      }""".stripMargin,
      u32 ->: (u32 x (u32 x u32)) ->: (u32 x (u32 x u32))
    )

    val update = fun(u32 ->: (u32 x u32) ->: u32)((dist, pair) =>
      dist + (pair._1 - pair._2) * (pair._1 - pair._2)
    )

    val select = fun(tuple => tuple._2._2)

    // p -> number of points, c -> number of clusters, f -> number of features
    // from lift.highLevel.kmeans featuresType = ArrayType(ArrayType(Float, P), F)
    //    features matrix F x P
    // from lift.highLevel.kmeans clustersType = ArrayType(ArrayType(Float, F), C)
    //    clusters matrix C X F
    val expr: ToBeTyped[Rise] = depFun((p: Nat, c: Nat, f: Nat) =>
      fun((p`.`f`.`u32) ->: (c`.`f`.`u32) ->: (p`.`u32))((features, clusters) =>
        gap8Run(8)(
          features |> mapPar(fun(feature =>
            clusters |> reduceSeq(fun(tuple => fun(cluster => {
              val dist = zip(feature)(cluster) |> reduceSeq(update)(cast(l(0)) :: u32)
              testF(dist)(tuple)
            })))(
              makePair(cast(l(4294967295L)) :: u32)(makePair(cast(l(0)) :: u32)(cast(l(0)) :: u32))
            ) |> select
          ))
        )
      )
    )

    val hostedModule = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(hostedModule)

    findDeviceBufferSync(3, code)
    checkCoreNumber(8, code)
    findParamsStruct("uint32_t*", 3, code)
    findParamsStruct("int", 3, code)
    //println(code)
  }

  test("Convolution 3x3") {
    val expr: ToBeTyped[Rise] = {
      depFun((w:Nat, h: Nat) =>
        fun((w`.`h`.`i16) ->: (3`.`3`.`i16) ->: ((w - 2)`.`(h - 2)`.`i16))((in, filter) =>
          gap8Run(8)(
            in |>
              slide2D(3, 1) |>
              mapPar(mapPar(fun(sub => {
                zip(sub |> join)(filter |> join) |>
                  map(fun(x => fst(x) * snd(x))) |>
                  reduceSeq(add)(li16(0))
              })))
          )
        )
      )
    }

    val module = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(module)

    findDeviceBufferSync(3, code)
    checkCoreNumber(8, code)
    findParamsStruct("int16_t*", 3, code)
    println(code)
  }
}
