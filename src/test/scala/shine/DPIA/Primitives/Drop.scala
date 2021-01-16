package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import util.gen

class Drop extends test_util.Tests {
  test ("Simple drop example") {
    val e = fun(ArrayType(128, int))(a => drop(8)(a) |> mapSeq(fun(x => x)))
    gen.CProgram(e)
  }
}
