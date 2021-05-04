package rise.eqsat

import Basic._
import rise.core.DSL._

class EtaReductionSubtleties extends test_util.Tests {
  test("missed eta-abstraction") {
    val start = Expr.fromNamed(introduceDataFuns(1, _ => f =>
      fun(x => fun(_ => f(0))(x)(x))))
    val goal = Expr.fromNamed(introduceDataFuns(1, _ => f =>
      f(0)))

    proveEquiv(start, goal,
      Seq(rules.eta, rules.beta),
      DefaultAnalysisWithFreeIntersection)

    try {
      proveEquiv(start, goal,
        Seq(rules.eta, rules.beta))
      assert(false)
    } catch {
      case CouldNotProveEquiv =>
    }
  }

  test("invalid eta-abstraction") {
    // FIXME: this is not a great example
    val start = Expr.fromNamed(introduceDataFuns(1, dts => f =>
      fun(dts(1))(z => fun(x => fun(_ => f(0))(x)(x)))))
    val goal = Expr.fromNamed(introduceDataFuns(1, dts => f =>
      fun(dts(1))(z => fun(_ => f(0))(z))))

    proveEquiv(start, goal,
      Seq(rules.eta, rules.beta),
      DefaultAnalysisWithFreeIntersection)

    try {
      proveEquiv(start, goal,
        Seq(rules.eta, rules.beta))
      assert(false)
    } catch {
      case CouldNotProveEquiv =>
    }
  }
}
