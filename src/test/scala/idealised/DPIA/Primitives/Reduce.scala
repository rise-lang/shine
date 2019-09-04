package idealised.DPIA.Primitives

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import idealised.util.gen
import lift.OpenCL.primitives.{mapLocal, oclReduceSeq, toPrivateFun}
import lift.arithmetic.Cst

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

  test("oclReduceSeq does no automatic copy of its initial accumulator value") {
    val zeros = nFun(n1 =>
            generate(fun(IndexType(n1))(_ => l(0.0f))))

    //TODO check that private memory array has constant size
    val e =
      nFun(m => nFun(n => fun(m`.`n`.`float)(arr2D => arr2D
        |> oclReduceSeq (AddressSpace.Private)
                        (fun(n`.`float ->: n`.`float ->: n`.`float)((acc, arr1D) =>
                          zip (acc) (arr1D) |> mapSeq (fun(t => t._1 + t._2))))
          (generate(fun(IndexType(n))(_ => l(0.0f))) |> toPrivateFun(mapSeq (fun(x => x)))))))

    gen.OpenCLKernel(e).code
  }
}
