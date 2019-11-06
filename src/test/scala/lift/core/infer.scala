package lift.core

import lift.core.DSL._
import lift.core.types._

class infer extends test_util.Tests {
  test("Infer int addition type") {
    val typed = infer(l(1) + l(2))
    assert(typed.t == int)
  }

  test("Infer partial int addition type") {
    val typed = infer(fun(x => l(1) + x))
    assert(typed.t == int ->: int)
  }

  test("infer int") {
    val a = infer(nFun(m => nFun(x =>
      zip(generate(fun(IndexType(x))(_ => l(0.0f))))(generate(fun(IndexType(x))(_ => l(0.0f))))
    )(m)))
    val b = infer(nFun(m => nFun(x =>
      zip(generate(fun(IndexType(x))(_ => l(0.0f))))(generate(fun(IndexType(m))(_ => l(0.0f))))
    )(m)))
    assert(true)
  }
}
