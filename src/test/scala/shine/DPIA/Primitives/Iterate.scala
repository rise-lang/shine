package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.types._
import util.gen

class Iterate extends shine.test_util.Tests {
  val add = fun(a => fun(b => a + b))

  test("Simple iterate example should generate syntactic valid C code ") {
    val e = fun(ArrayType(128, int))(a =>
      a |> iterate(6)(nFun(_ => split(2) >> mapSeq(reduceSeq(add)(l(0))))))

    gen.CProgram(e)
  }
}
