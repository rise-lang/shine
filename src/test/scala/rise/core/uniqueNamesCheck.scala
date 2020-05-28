package rise.core

import rise.core.DSL._
import rise.core.types._

class uniqueNamesCheck extends rise.testUtil.Tests {
  test("beta-reducing holds unique names") {
    val f = fun(x => x(l(1)) + x(l(1)))
    val e = fun(y => y + l(1))
    assert(uniqueNames.check(infer(f(e))))
    assert(uniqueNames.check(infer(lifting.liftFunExpr(f).reducing(e))))
  }

  test("type inference enforces unique names") {
    val e = fun(y => y + l(1))
    val fe = e(l(1)) + e(l(1))
    assert(uniqueNames.check(infer(fe)))
  }
}
