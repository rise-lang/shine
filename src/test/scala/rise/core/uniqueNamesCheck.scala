package rise.core

import rise.core.TypedDSL._

class uniqueNamesCheck extends test_util.Tests {
  test("beta-reducing holds unique names") {
    val f = fun(x => x(l(1)) + x(l(1)))
    val e = fun(y => y + l(1))
    assert(uniqueNames.check(uniqueNames.enforce(f(e).toExpr)))
    assert(uniqueNames.check(uniqueNames.enforce(lifting.liftFunExpr(f).reducing(e))))
  }

  test("type inference enforces unique names") {
    val e = fun(y => y + l(1))
    val fe = e(l(1)) + e(l(1))
    assert(uniqueNames.check(uniqueNames.enforce(fe.toExpr)))
  }
}
