package shine.DPIA.Primitives

import rise.core.Lambda
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import util.gen
import util.gen.c.function

class Iterate extends test_util.Tests {
  val add: ToBeTyped[Lambda] = fun(a => fun(b => a + b))

  test("Simple iterate example should generate syntactic valid C code ") {
    val e = fun(ArrayType(128, int))(a =>
      a |> iterate(6)(depFun((_: Nat) => split(2) >> mapSeq(reduceSeq(add)(l(0))))))

    function.asStringFromExpr("iterate")(e)
  }
}
