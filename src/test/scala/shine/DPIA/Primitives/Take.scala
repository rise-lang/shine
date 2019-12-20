package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.types._
import rise.core.primitives._
import util.gen

class Take extends test_util.Tests {

  test ("Simple take example") {
    val e = fun(ArrayType(128, int))(a =>
      take(8)(a) |> mapSeq(fun(x => x)))

    gen.CProgram(e)
  }

  ignore ("Trigger TakeAcc acceptor translation, what should happen?") {
    val e = fun(ArrayType(128, int))(a => take(8)(a))

    gen.CProgram(e)
  }

}
