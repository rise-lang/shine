package idealised.OpenCL

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.primitives._
import util.gen

class To extends test_util.Tests {
  val id = fun(x => x)
  val add1 = fun(x => x + l(1.0f))

  test("generate OpenCL code with array in private memory") {
    val e = nFun(n => fun(ArrayType(n, ArrayType(3, float)))(a =>
      a |> mapGlobal(toPrivateFun(mapSeq(add1)) >> mapSeq(id))
    ))

    val code = gen.OpenCLKernel(e).code
    "for \\(".r.findAllIn(code).length shouldBe 1
  }

  test("toGlobal inside mapSeq") {
    val e = nFun(n => fun(ArrayType(n, ArrayType(3, float)))(a =>
      a |> mapSeq(toGlobalFun(mapSeq(add1)) >> mapSeq(id))
    ))

    val code = gen.OpenCLKernel(e).code
    // `3.float` should be allocated, indexing should not depend on the outer loop
    // this assumes that there is only one thread running because there is no parallel map
  }

  test("toGlobal inside mapGlobal") {
    val e = nFun(n => fun(ArrayType(n, ArrayType(3, float)))(a =>
      a |> mapGlobal(toGlobalFun(mapSeq(add1)) >> mapSeq(id))
    ))

    val code = gen.OpenCLKernel(e).code
    // `max(nthreads, n).3.float` should be allocated, indexing should depend on get_global_id(0)
  }

  test("toGlobal inside mapSeq inside toGlobal inside mapGlobal") {
    val e = nFun(n => fun(ArrayType(n, ArrayType(3, float)))(a =>
      a |> mapGlobal(
        toGlobalFun(mapSeq(toGlobalFun(add1) >> add1)) >>
        mapSeq(id)
      )
    ))

    val code = gen.OpenCLKernel(e).code
    // first inner loop, first memory write:
    // `max(nthreads, n).float` should be allocated, indexing should depend on get_global_id(0)
    // first inner loop, second memory write:
    // `max(nthreads, n).3.float` should be allocated, indexing should depend on get_global_id(0)
  }

  test("arithmetic expressions should be simplified when unrolling private arrays") {
    val e = fun(ArrayType(1, float))(a =>
      a |> padCst(1)(1)(l(1.0f)) |> toPrivateFun(mapSeq(id)) |> mapSeq(id)
    )

    val code = gen.OpenCLKernel(e).code
    "for \\(".r.findAllIn(code).length shouldBe 0
    " \\? ".r.findAllIn(code).length shouldBe 0
  }
}
