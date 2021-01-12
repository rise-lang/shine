package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import util.gen
import util.gen.c.function

class Take extends test_util.Tests {

  test ("Simple take example") {
    val e = fun(ArrayType(128, int))(a =>
      take(8)(a) |> mapSeq(fun(x => x)))

    function.asStringFromExpr("take")(e)
  }

  ignore ("Trigger TakeAcc acceptor translation, what should happen?") {
    val e = fun(ArrayType(128, int))(a => take(8)(a))

    function.asStringFromExpr("take")(e)
  }

}
