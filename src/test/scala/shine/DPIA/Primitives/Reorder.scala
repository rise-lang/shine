package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.types._
import rise.core.HighLevelConstructs.reorderWithStride
import util.gen

import arithexpr.arithmetic.Cst

class Reorder extends shine.test_util.Tests {
  test("Simple gather example should generate syntactic valid C code with two one loops") {
    val e = nFun(n => fun(ArrayType(n, f32))(xs =>
      xs |> reorderWithStride(128) |> mapSeq(fun(x => x))
    ))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Simple 2D gather example should generate syntactic valid C code with two two loops") {
    val e = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, f32)))(xs =>
      xs |> map(reorderWithStride(128)) |> mapSeq(mapSeq(fun(x => x)))
    )))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple scatter example should generate syntactic valid C code with two one loops") {
    val e = nFun(n => fun(ArrayType(n, f32))(xs =>
      xs |> mapSeq(fun(x => x)) |> reorderWithStride(Cst(128))
    ))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Simple 2D scatter example should generate syntactic valid C code with two two loops") {
    val e = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, f32)))(xs =>
      xs |> mapSeq(mapSeq(fun(x => x))) |> map(reorderWithStride(Cst(128)))
    )))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 2
  }
}
