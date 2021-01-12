package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import util.gen
import util.gen.c.function

class Map extends test_util.Tests {
  test("Simple 1D map example should generate syntactic valid C code with one for loop") {
    val e =
      depFun((n: Nat) => fun(ArrayType(n, f32))(xs =>
        xs |> mapSeq(fun(x => x))))

    val code = function.asStringFromExpr("map")(e)

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Simple 2D map example should generate syntactic valid C code with two for loop") {
    val e =
      depFun((n: Nat, m: Nat) => fun(ArrayType(n, ArrayType(m, f32)))(xs =>
        xs |> mapSeq(mapSeq(fun(x => x)))))

    val code = function.asStringFromExpr("map")(e)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 3D map example should generate syntactic valid C code with three for loop") {
    val e =
      depFun((n: Nat, m: Nat, o: Nat) =>
        fun(ArrayType(n, ArrayType(m, ArrayType(o, f32))))(xs =>
          xs |> mapSeq(mapSeq(mapSeq(fun(x => x))))))

    val code = function.asStringFromExpr("map")(e)

    "for".r.findAllIn(code).length shouldBe 3
  }
}
