package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import HighLevelConstructs.reorderWithStride
import util.gen
import arithexpr.arithmetic.Cst
import util.gen.c.function

class Reorder extends test_util.Tests {
  test("Simple gather example should generate syntactic valid C code with two one loops") {
    val e = depFun((n: Nat) => fun(ArrayType(n, f32))(xs =>
      xs |> reorderWithStride(128) |> mapSeq(fun(x => x))
    ))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Simple 2D gather example should generate syntactic valid C code with two two loops") {
    val e = depFun((n: Nat, m: Nat) => fun(ArrayType(n, ArrayType(m, f32)))(xs =>
      xs |> map(reorderWithStride(128)) |> mapSeq(mapSeq(fun(x => x)))
    ))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple scatter example should generate syntactic valid C code with two one loops") {
    val e = depFun((n: Nat) => fun(ArrayType(n, f32))(xs =>
      xs |> mapSeq(fun(x => x)) |> reorderWithStride(Cst(128))
    ))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Simple 2D scatter example should generate syntactic valid C code with two two loops") {
    val e = depFun((n: Nat, m: Nat) => fun(ArrayType(n, ArrayType(m, f32)))(xs =>
      xs |> mapSeq(mapSeq(fun(x => x))) |> map(reorderWithStride(Cst(128)))
    ))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 2
  }
}
