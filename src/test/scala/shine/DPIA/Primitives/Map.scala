package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.types._
import rise.core.DSL.mapSeq
import util.gen

class Map extends shine.test_util.Tests {
  test("Simple 1D map example should generate syntactic valid C code with one for loop") {
    val e =
      nFun(n => fun(ArrayType(n, f32))(xs =>
        xs |> mapSeq(fun(x => x))))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Simple 2D map example should generate syntactic valid C code with two for loop") {
    val e =
      nFun(n => nFun(m => fun(ArrayType(n, ArrayType(m, f32)))(xs =>
        xs |> mapSeq(mapSeq(fun(x => x))))))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 3D map example should generate syntactic valid C code with three for loop") {
    val e =
      nFun(n => nFun(m => nFun(o =>
        fun(ArrayType(n, ArrayType(m, ArrayType(o, f32))))(xs =>
          xs |> mapSeq(mapSeq(mapSeq(fun(x => x))))))))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 3
  }
}
