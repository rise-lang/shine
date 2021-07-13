package shine.GAP8

import rise.GAP8.DSL.gap8Run
import rise.core.DSL.HighLevelConstructs._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types._
import rise.elevate.Rise
import shine.GAP8

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

      //.count(line => line.contains(typeName))
      //.shouldBe(count)
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

    println(code)

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

    println(code)
  }

  test("Matmul") {
    val expr: ToBeTyped[Expr] = depFun((n: Nat, m: Nat, o: Nat) =>
      fun((n`.`o`.`f32) ->: (o`.`m`.`f32) ->: (n`.`m`.`f32))((a, b) =>
        gap8Run(8)(
          a |> mapSeq(fun(rowa =>
            b |> transpose |> mapSeq(fun(colb =>
              zip(rowa)(colb) |> map(fun(x => fst(x) * snd(x))) |> reduceSeq(add)(lf32(0.0f))
            ))
          ))
        )
      )
    )

    val hostedModule = util.gen.gap8.hosted.fromExpr(expr)
    val code = GAP8.Module.translateToString(hostedModule)

    findDeviceBufferSync(3, code)
    checkCoreNumber(8, code)
    findParamsStruct("float*", 3, code)
    findParamsStruct("int", 3, code)

    println(code)
  }

  test("Sobel filter on GAP8") {
    val expr: ToBeTyped[Rise] = depFun((n: Nat, m: Nat) =>
      fun((n`.`m`.`u8) ->: (3`.`3`.`int) ->: (3`.`3`.`int) ->: (n`.`m`.`u8))((pic, h_w, v_w) =>
        gap8Run(8)(
          pic |>
            padClamp2D(l = 1, r = 1) |>
            slide2D(sz = 3, st = 1) |>
            mapSeq(mapSeq(fun(submat => {
              zip(submat |> join)(h_w |> join) |> map(fun(x => (cast(fst(x)) :: u32) * cast(snd(x)) :: u32)) |> reduceSeq(add)(cast(l(0)) :: u32) |> letf(h =>
                zip(submat |> join)(v_w |> join) |> map(fun(x => (cast(fst(x)) :: u32) * cast(snd(x)) :: u32)) |> reduceSeq(add)(cast(l(0)) :: u32) |> letf(v =>
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

    //println(code)

    findDeviceBufferSync(4, code)
    checkCoreNumber(8, code)
    findParamsStruct("uint8_t*", 2, code)
    findParamsStruct("int", 2, code)
    findParamsStruct("int*", 2, code)

  }
}
