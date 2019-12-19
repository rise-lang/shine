package rise.core

import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._

class infer extends test_util.Tests {
  test("Infer int addition type") {
    val typed = infer(l(1) + l(2))
    assert(typed.t == int)
  }

  test("Infer partial int addition type") {
    val typed = infer(fun(x => l(1) + x))
    assert(typed.t == int ->: int)
  }
}
