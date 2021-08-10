package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import util.gen.c.function

class Split extends test_util.Tests {

  test("Simple split example should generate syntactic valid C code with two for loops") {
    val e =
      depFun((n: Nat) => fun(ArrayType(n, f32))(xs =>
        xs |> split(2) |> mapSeq(mapSeq(fun(x => x)))))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 2D split example with separate maps should generate syntactic valid OpenMP code with three for loops") {
    val e =
      depFun((n: Nat, m: Nat) =>
        fun(ArrayType(n, ArrayType(m, f32)))(xs =>
          xs |> map(split(2)) |> mapSeq(mapSeq(mapSeq(fun(x => x))))
        ))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 3
  }

  test("Simple 2D split example with merged maps should generate syntactic valid OpenMP code with three for loops") {
    val e =
      depFun((n: Nat, m: Nat) =>
        fun(ArrayType(n, ArrayType(m, f32)))( xs =>
          xs |> mapSeq(split(2) >> mapSeq(mapSeq(fun(x => x))))
        ))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 3
  }
}
