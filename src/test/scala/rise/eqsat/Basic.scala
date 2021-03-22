package rise.eqsat

import rise.core.DSL._
import rise.core.primitives._

class Basic extends test_util.Tests {
  import Basic.proveEquiv

  test("normalize") {
    import ExprDSL._

    assert(
      BENF(lam(app(app(lam(app(map, %(0))), %(2)), %(0)))) ==
      app(map, %(1)))
  }

  test("map fusion") {
    proveEquiv(
      fun(f1 => fun(f2 => fun(f3 => fun(f4 =>
        map(f1) >> map(f2) >> map(f3) >> map(f4))))),
      fun(f1 => fun(f2 => fun(f3 => fun(f4 =>
        map(f1 >> f2) >> map(f3 >> f4))))),
      Seq(rules.eta, rules.beta, rules.mapFusion)
    )
  }

  test("map fission") {
    proveEquiv(
      fun(f1 => fun(f2 => fun(f3 => fun(f4 =>
        map(f1 >> f2 >> f3 >> f4))))),
      fun(f1 => fun(f2 => fun(f3 => fun(f4 =>
        map(f1) >> map(f2) >> map(f3) >> map(f4))))),
      Seq(rules.eta, rules.beta, rules.mapFission)
    )
  }

  test("map first fission") {
    proveEquiv(
      fun(f1 => fun(f2 => fun(f3 => fun(f4 =>
        map(f1 >> f2 >> f3 >> f4))))),
      fun(f1 => fun(f2 => fun(f3 => fun(f4 =>
        map(f1) >> map(f2 >> f3 >> f4))))),
      Seq(rules.eta, rules.beta, rules.mapFission, rules.mapFusion)
    )
  }
}

object Basic {
  def proveEquiv(start: rise.core.Expr,
                 goal: rise.core.Expr,
                 rules: Seq[Rewrite[DefaultAnalysisData]]): Unit = {
    val normStart = BENF(Expr.fromNamed(start))
    val normGoal = BENF(Expr.fromNamed(goal))
    println(s"normalized start: ${Expr.toNamed(normStart)}")
    println(s"normalized goal: ${Expr.toNamed(normGoal)}")
    val goalPattern = Pattern.fromExpr(normGoal).compile()

    val runner = Runner.withAnalysis(DefaultAnalysis)
    val id = runner.egraph.addExpr(normStart)
    runner.doneWhen { r =>
      goalPattern.searchEClass(r.egraph, id).isDefined
    }
    runner.run(rules)
    runner.printReport()

    if (!runner.stopReasons.contains(Done)) {
      runner.iterations.foreach(println)
      assert(false)
    }
  }
}
