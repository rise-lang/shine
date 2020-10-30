package rise.core

import rise.core.TypedDSL._
import rise.core.TypeLevelDSL._
import rise.core.types._

class infer extends test_util.Tests {
  test("Infer int addition type") {
    val typed = (l(1) + l(2)).toExpr
    assert(typed.t == int)
    assert(check(typed).isSuccess)
  }

  test("Infer partial int addition type") {
    val typed = (fun(x => l(1) + x)).toExpr
    assert(typed.t == int ->: int)
    assert(check(typed).isSuccess)
  }
}
