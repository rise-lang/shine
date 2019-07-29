package idealised.DPIA.Primitives

import idealised.util.gen
import lift.core.DSL._
import lift.core.primitives._
import lift.core.types._

class DepMap extends idealised.util.Tests {
  test("Simple triangle depMap example should generate syntactic valid C code with two for loop") {
    val e =
      nFun(n => fun(DepArrayType(n, n2dtFun(i => (i+1)`.`float)) ->: DepArrayType(n, n2dtFun(i => (i+1)`.`float)))(xs =>
        xs |> depMapSeq(nFun(_ => mapSeq(fun(x => x))))))

    println(e)

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 3D map example should generate syntactic valid C code with three for loop") {
    val e =
      nFun(n => nFun(m => nFun(o =>
        fun(n`.`(m`.`(o`.`float)))(xs =>
          xs |> mapSeq(mapSeq(mapSeq(fun(x => x))))))))

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 3
  }
}
