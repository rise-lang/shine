package idealised.OpenCL

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import util.gen

class ArrayToPrivate extends test_util.Tests {
  test("generate OpenCL code with array in private memory") {
    import lift.OpenCL.DSL._

    val e = nFun(n => fun(ArrayType(n, ArrayType(3, float)))(a =>
      a |> mapGlobal(fun(y => toPrivate(mapSeq(fun(x => x + l(1.0f)))(y)) |> mapSeq(fun(x => x))))
    ))

    val code = gen.OpenCLKernel(e).code
    "for \\(".r.findAllIn(code).length shouldBe 1
  }

  test("arithmetic expressions should be simplified when unrolling private arrays") {
    import lift.OpenCL.DSL._

    val e = fun(ArrayType(1, float))(a =>
      a |> padCst(1)(1)(l(1.0f)) |> fun(y => toPrivate(mapSeq(fun(x => x))(y))) |> mapSeq(fun(x => x))
    )

    val code = gen.OpenCLKernel(e).code
    "for \\(".r.findAllIn(code).length shouldBe 0
    " \\? ".r.findAllIn(code).length shouldBe 0
  }
}