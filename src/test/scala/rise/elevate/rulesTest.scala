package rise.elevate

import elevate.core._
import rise.core.TypedDSL._
import rise.elevate.rules._
import rise.elevate.rules.traversal._

class rulesTest extends test_util.Tests {
  def rewriteStep(a: Rise, s: Strategy[Rise], b: Rise): Unit = {
    assert(s(a).get == b)
  }

  test("simple beta reduction") {
    val one = fun(x => x)(l(1))
    val two = fun(y => fun(x => x + y))(l(1))(l(2))
    rewriteStep(one, betaReduction, l(1))
    rewriteStep(two, function(betaReduction), fun(x => x + l(1))(l(2)))
    rewriteStep(two, function(betaReduction) `;` betaReduction, l(2) + l(1))
  }
}
