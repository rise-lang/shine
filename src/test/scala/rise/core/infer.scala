package rise.core

import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.types.DataType._

class infer extends test_util.Tests {
  test("Infer int addition type") {
    val typed = (l(1) + l(2)).toExpr
    assert(typed.t =~= int)
    assert(check(typed).isSuccess)
  }

  test("Infer partial int addition type") {
    val typed = (fun(x => l(1) + x)).toExpr
    assert(typed.t =~= int ->: int)
    assert(check(typed).isSuccess)
  }
}
