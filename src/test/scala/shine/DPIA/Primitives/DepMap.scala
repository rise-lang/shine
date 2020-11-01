package shine.DPIA.Primitives

import rise.core.dsl.Type._
import rise.core.dsl._
import rise.core.exprs.primitives._
import rise.core.types._
import rise.core.util.gen

class DepMap extends test_util.Tests {
  ignore("Simple triangle depMap example should generate syntactic valid C code with two for loop") {
    val e =
      depFun((n: Nat) => fun(DepArrayType(n, n2dtFun(i => (i+1)`.`f32)) ->: DepArrayType(n, n2dtFun(i => (i+1)`.`f32)))(xs =>
        xs |> depMapSeq(depFun((_: Nat) => mapSeq(fun(x => x))))))

    println(e)

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 3D map example should generate syntactic valid C code with three for loop") {
    val e =
      depFun((n: Nat) => depFun((m: Nat) => depFun((o: Nat) =>
        fun(n`.`(m`.`(o`.`f32)))(xs =>
          xs |> mapSeq(mapSeq(mapSeq(fun(x => x))))))))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 3
  }
}
