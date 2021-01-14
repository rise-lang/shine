package shine.DPIA.Primitives

import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import util.gen
import util.gen.c.function

class DepMap extends test_util.Tests {
  ignore("Simple triangle depMap example should generate syntactic valid C code with two for loop") {
    val e =
      depFun((n: Nat) => fun(DepArrayType(n, n2dtFun(i => (i+1)`.`f32)) ->: DepArrayType(n, n2dtFun(i => (i+1)`.`f32)))(xs =>
        xs |> depMapSeq(depFun((_: Nat) => mapSeq(fun(x => x))))))

    println(e)

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 3D map example should generate syntactic valid C code with three for loop") {
    val e =
      depFun((n: Nat, m: Nat, o: Nat) =>
        fun(n`.`(m`.`(o`.`f32)))(xs =>
          xs |> mapSeq(mapSeq(mapSeq(fun(x => x))))))

    val code = function.asStringFromExpr(e)

    "for".r.findAllIn(code).length shouldBe 3
  }
}
