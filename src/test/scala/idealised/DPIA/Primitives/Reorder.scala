package idealised.DPIA.Primitives

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.core.HighLevelConstructs.reorderWithStride
import idealised.util.gen

import lift.arithmetic.Cst

class Reorder extends idealised.util.Tests {
  test("Simple gather example should generate syntactic valid C code with two one loops") {
    val e = nFun(n => fun(ArrayType(n, float))(xs =>
      xs |> reorderWithStride(128) |> mapSeq(fun(x => x))
    ))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Simple 2D gather example should generate syntactic valid C code with two two loops") {
    val e = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, float)))(xs =>
      xs |> map(reorderWithStride(128)) |> mapSeq(mapSeq(fun(x => x)))
    )))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple scatter example should generate syntactic valid C code with two one loops") {
    val e = nFun(n => fun(ArrayType(n, float))(xs =>
      xs |> mapSeq(fun(x => x)) |> reorderWithStride(Cst(128))
    ))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Simple 2D scatter example should generate syntactic valid C code with two two loops") {
    val e = nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, float)))(xs =>
      xs |> mapSeq(mapSeq(fun(x => x))) |> map(reorderWithStride(Cst(128)))
    )))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 2
  }
}
