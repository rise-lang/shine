package idealised.DPIA.Primitives

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives.{reduceSeq, map, mapSeq}
import idealised.util.gen

class Reduce extends idealised.util.Tests {
  val add = fun(a => fun(b => a + b))

  test("Simple example should generate syntactic valid C code with one loop") {
    val e =
      nFun(n => fun(ArrayType(n, float))(a =>
        a |> reduceSeq(add)(l(0.0f))))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Fusing a reduce into a map should generate syntactic valid C code") {
    val e =
      nFun(h => nFun(w =>
        fun(ArrayType(h, ArrayType(w, float)))(a =>
          a |> map(reduceSeq(add)(l(0.0f))) |> mapSeq(fun(x => x))
        )))

    gen.CProgram(e)
  }

  test("Fusing a reduce into another should generate syntactic valid C code with two loops") {
    val e =
      nFun(h => nFun(w =>
        fun(ArrayType(h, ArrayType(w, float)))(a =>
          a |> map(reduceSeq(add)(l(0.0f))) |> reduceSeq(add)(l(0.0f))
      )))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Fusing a reduce into a map from the other side should generate syntactic valide C code") {
    val e =
      nFun(h => nFun(w =>
        fun(ArrayType(h, ArrayType(w, float)))(a =>
          a |> mapSeq(mapSeq(fun(x => x))) |> map(reduceSeq(add)(l(0.0f)))
        )))

    gen.CProgram(e).code
  }
}
