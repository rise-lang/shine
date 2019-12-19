package idealised.DPIA.Primitives

import rise.core.DSL._
import rise.core.types._
import rise.core.primitives._
import util.gen

class Split extends test_util.Tests {

  test("Simple split example should generate syntactic valid C code with two for loops") {
    val e =
      nFun(n => fun(ArrayType(n, float))(xs =>
        xs |> split(2) |> mapSeq(mapSeq(fun(x => x)))))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 2D split example with separate maps should generate syntactic valid OpenMP code with three for loops") {
    val e =
      nFun(n => nFun(m =>
        fun(ArrayType(n, ArrayType(m, float)))(xs =>
          xs |> map(split(2)) |> mapSeq(mapSeq(mapSeq(fun(x => x))))
        )))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 3
  }

  test("Simple 2D split example with merged maps should generate syntactic valid OpenMP code with three for loops") {
    val e =
      nFun(n => nFun(m =>
        fun(ArrayType(n, ArrayType(m, float)))( xs =>
          xs |> mapSeq(split(2) >> mapSeq(mapSeq(fun(x => x))))
        )))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 3
  }
}
