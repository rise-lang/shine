package idealised.DPIA.Primitives

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import idealised.util.gen

class Iterate extends idealised.util.Tests {
  val add = fun(a => fun(b => a + b))
  test("Simple iterate example should generate syntactic valid C code ") {
    val e = fun(ArrayType(128, int))(a =>
      a |> iterate(6)(nFun(_ => split(2) >> mapSeq(reduceSeq(add)(l(0))))))

    gen.CProgram(e)
  }
}
