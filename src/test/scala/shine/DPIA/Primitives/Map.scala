package shine.DPIA.Primitives

import rise.core.dsl._
import rise.core.exprs.primitives._
import rise.core.types._
import rise.core.util.gen

class Map extends test_util.Tests {
  test("Simple 1D map example should generate syntactic valid C code with one for loop") {
    val e =
      depFun((n: Nat) => fun(ArrayType(n, f32))(xs =>
        xs |> mapSeq(fun(x => x))))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Simple 2D map example should generate syntactic valid C code with two for loop") {
    val e =
      depFun((n: Nat) => depFun((m: Nat) => fun(ArrayType(n, ArrayType(m, f32)))(xs =>
        xs |> mapSeq(mapSeq(fun(x => x))))))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 3D map example should generate syntactic valid C code with three for loop") {
    val e =
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
        fun(ArrayType(n, ArrayType(m, ArrayType(o, f32))))(xs =>
          xs |> mapSeq(mapSeq(mapSeq(fun(x => x))))))))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 3
  }
}
