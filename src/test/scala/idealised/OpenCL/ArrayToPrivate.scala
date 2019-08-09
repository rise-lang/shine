package idealised.OpenCL

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import idealised.util.gen

class ArrayToPrivate extends idealised.util.TestsWithExecutor {
  test("generate OpenCL code with array in private memory") {
    import lift.OpenCL.primitives._

    val e = nFun(n => fun(ArrayType(n, ArrayType(3, float)))(a =>
      a |> mapGlobal(fun(y => toPrivate(mapSeq(fun(x => x + l(1.0f)))(y)) |> mapSeq(fun(x => x))))
    ))

    val code = gen.OpenCLKernel(e).code
    "for \\(".r.findAllIn(code).length shouldBe 1
  }
}