package lift.core

import lift.core.DSL._
import lift.core.TypeLevelDSL._
import lift.core.types._

class infer extends test_util.Tests {
  test("Infer int addition type") {
    val typed = infer(l(1) + l(2))
    assert(typed.t == int)
  }

  test("Infer partial int addition type") {
    val typed = infer(fun(x => l(1) + x))
    assert(typed.t == int ->: int)
    val a = NatIdentifier("a")
    val b = NatIdentifier("b")
    val c = NatIdentifier("c")
    val d = NatIdentifier("d")
    val le = a + (b * d) + (-1 * c)
    val re = a + (-1 * c) + (b * d)
    assert(le == re)
  }
}
