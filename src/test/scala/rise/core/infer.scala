package rise.core

import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._

class infer extends rise.test_util.Tests {
  test("Infer int addition type") {
    val typed = infer(l(1) + l(2))
    assert(typed.t == int)
  }

  test("Infer partial int addition type") {
    val typed = infer(fun(x => l(1) + x))
    assert(typed.t == int ->: int)
  }

  test("Type inference should be idempotent") {
    val add: Expr = dtFun(dt0 => dtFun(dt1 => fun(
      dt0 ->: dt0 ->: dt0
    )((a, b) => {
      cast((cast(a) :: dt1) + (cast(b) :: dt1)) :: dt0
    })))

    val once: Expr = infer(add)

    val twice: Expr = infer(once)

    assert(IsClosedForm(once))

    assert(IsClosedForm(twice))

    assert(once == twice)
  }
}
