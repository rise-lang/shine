package elevate.rise

import elevate.core._
import elevate.rise.rules._
import elevate.rise.rules.traversal._
import rise.core.TypedDSL._

class rulesTest extends shine.test_util.Tests {
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
