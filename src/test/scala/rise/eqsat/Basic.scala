package rise.eqsat

import rise.{core => rc}
import rise.core.{types => rct}

class Basic extends test_util.Tests {
  import Basic.proveEquiv

  test("normalize") {
    import ExprDSL._

    val m = map((f32 ->: f32) ->: (`%n`(0)`.`f32) ->: (`%n`(0)`.`f32))
    assert(
      BENF(lam(`%n`(0)`.`f32,
        app(app(lam(f32 ->: f32, app(m, %(0, f32 ->: f32))), %(2, f32 ->: f32)),
          %(0, `%n`(0)`.`f32)))) ==
      app(m, %(1, f32 ->: f32)))
  }

  import rise.core.DSL._
  import rise.core.DSL.Type._
  import rise.core.primitives._

  def introduceDataFuns(n: Int, k: Seq[rct.DataType] => Seq[ToBeTyped[rc.Identifier]] => ToBeTyped[rc.Expr]): ToBeTyped[rc.Expr] = {
    def recDataTypes(n: Int, k: Seq[rct.DataType] => ToBeTyped[rc.Expr]): ToBeTyped[rc.Expr] =
      if (n <= 0) {
        k(Seq())
      } else {
        recDataTypes(n - 1, rest => depFun((dt: rct.DataType) => k(dt +: rest)))
      }

    def recFuns(n: Int, dts: Seq[rct.DataType], k: Seq[ToBeTyped[rc.Identifier]] => ToBeTyped[rc.Expr]): ToBeTyped[rc.Expr] =
      if (n <= 0) {
        k(Seq())
      } else {
        recFuns(n - 1, dts, rest => fun((dts(n) ->: dts(n - 1)): rct.Type)(f => k(f +: rest)))
      }

    recDataTypes(n + 1, dts => recFuns(n, dts, k(dts)))
  }

  test("introduceDataFuns") {
    val e = introduceDataFuns(4, _ => f => {
      f(0) >> f(1) >> f(2) >> f(3)
    })
    println(e.toExpr)
  }

  def withArrayAndFuns(n: Int, k: ToBeTyped[rc.Identifier] => Seq[ToBeTyped[rc.Identifier]] => ToBeTyped[rc.Expr]): ToBeTyped[rc.Expr] = {
    impl { elemT: rct.DataType =>
      depFun((size: rct.Nat) => introduceDataFuns(n, _ => f => fun(size`.`elemT)(in => k(in)(f)))) }
  }

  test("map fusion") {
    proveEquiv(
      withArrayAndFuns(4, in => f =>
        in |> map(f(0)) |> map(f(1)) |> map(f(2)) |> map(f(3))),
      withArrayAndFuns(4, in => f =>
        in |> map(f(0) >> f(1)) |> map(f(2) >> f(3))),
      Seq(rules.eta, rules.beta, rules.mapFusion)
    )
  }

  test("map fission") {
    proveEquiv(
      withArrayAndFuns(4, in => f =>
        in |> map(f(0) >> f(1) >> f(2) >> f(3))),
      withArrayAndFuns(4, in => f =>
        in |> map(f(0)) |> map(f(1)) |> map(f(2)) |> map(f(3))),
      Seq(rules.eta, rules.beta, rules.mapFission)
    )
  }

  test("map first fission") {
    proveEquiv(
      withArrayAndFuns(4, in => f =>
        in |> map(f(0) >> f(1) >> f(2) >> f(3))),
    withArrayAndFuns(4, in => f =>
        in |> map(f(0)) >> map(f(1) >> f(2) >> f(3))),
      Seq(rules.eta, rules.beta, rules.mapFission, rules.mapFusion)
    )
  }

  test("slide movements") {
    /* TODO:
    proveEquiv(
        map(f1 >> map(slide(3)(1)) >> map(slide(4)(2)) >> map(slide(5)(3)) >> map(map(map(map(map(f2)))))),
        map(f1 >> map(f2) >> map(slide(3)(1) >> slide(4)(2) >> slide(5)(3))),
      Seq(rules.eta, rules.beta, rules.mapFusion, rules.mapFission, rules.slideBeforeMapMapF)
    )*/
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
