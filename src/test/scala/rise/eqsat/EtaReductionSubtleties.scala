package rise.eqsat

import Basic._
import rise.core.DSL._
import ProveEquiv.syntax._

class EtaReductionSubtleties extends test_util.Tests {
  test("missed eta reduction") {
    val start = Expr.fromNamed(introduceDataFuns(1, _ => f =>
      fun(x => fun(_ => f(0))(x)(x))))
    val goal = Expr.fromNamed(introduceDataFuns(1, _ => f =>
      f(0)))

    // note: breaks advanced Var TypeCheck
    // note: saturates without finding the goal with rules.beta
    ProveEquiv.init()
      .run(start, goal, Seq(rules.etaWithFreeIntersection, rules.betaExtract), Seq())

    try {
      ProveEquiv.init()
        .run(start, goal, Seq(rules.eta, rules.betaExtract), Seq())
      assert(false)
    } catch {
      case CouldNotProveEquiv =>
    }
  }

  // FIXME:
  //  1. this is not a great example
  //  2. does not trigger an invalid reduction anymore,
  //     due to the extraction-based index shifting
  ignore("invalid eta reduction") {
    val start = Expr.fromNamed(introduceDataFuns(1, dts => f =>
      fun(dts(0))(z => fun(x => fun(_ => f(0))(x)(x)))))
    val goal = Expr.fromNamed(introduceDataFuns(1, dts => f =>
      fun(dts(0))(z => fun(_ => f(0))(z))))

    // note: breaks advanced Var TypeCheck
    // note: saturates without finding the goal with rules.beta

    ProveEquiv.init()
      .run(start, goal, Seq(rules.etaWithFreeIntersection, rules.betaExtract), Seq())

    try {
      ProveEquiv.init()
        .run(start, goal, Seq(rules.eta, rules.betaExtract), Seq())
      assert(false)
    } catch {
      case CouldNotProveEquiv =>
    }
  }
}
