package rise.eqsat

import rise.{core => rc}
import rise.core.{types => rct}
import ProveEquiv.syntax._

class Basic extends test_util.Tests {
  import Basic._

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

  test("introduceDataFuns") {
    val e = introduceDataFuns(4, _ => f => {
      f(0) >> f(1) >> f(2) >> f(3)
    })
    println(e.toExpr)
  }

  test("map fusion") {
    ProveEquiv.init().runBENF(
      withArrayAndFuns(4, in => f =>
        in |> map(f(0)) |> map(f(1)) |> map(f(2)) |> map(f(3))),
      withArrayAndFuns(4, in => f =>
        in |> map(f(0) >> f(1)) |> map(f(2) >> f(3))),
      Seq(rules.eta, rules.beta, rules.mapFusion)
    )
  }

  test("map fusion (CNF)") {
    ProveEquiv.init().runCNF(
      withArrayAndFuns(4, in => f =>
        in |> map(f(0)) |> map(f(1)) |> map(f(2)) |> map(f(3))),
      withArrayAndFuns(4, in => f =>
        in |> map(f(0) >> f(1)) |> map(f(2) >> f(3))),
      Seq(rules.combinatory.compositionAssoc1,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.mapFusion)
    )
  }

  test("map fission") {
    ProveEquiv.init().runBENF(
      withArrayAndFuns(4, in => f =>
        in |> map(f(0) >> f(1) >> f(2) >> f(3))),
      withArrayAndFuns(4, in => f =>
        in |> map(f(0)) |> map(f(1)) |> map(f(2)) |> map(f(3))),
      Seq(rules.eta, rules.betaExtract, rules.mapFission)
    )
  }

  test("map fission (CNF)") {
    ProveEquiv.init().runCNF(
      withArrayAndFuns(4, in => f =>
        in |> map(f(0) >> f(1) >> f(2) >> f(3))),
      withArrayAndFuns(4, in => f =>
        in |> map(f(0)) |> map(f(1)) |> map(f(2)) |> map(f(3))),
      Seq(rules.combinatory.compositionAssoc1,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.mapFission)
    )
  }

  test("map first fission") {
    ProveEquiv.init().runBENF(
      withArrayAndFuns(4, in => f =>
        in |> map(f(0) >> f(1) >> f(2) >> f(3))),
      withArrayAndFuns(4, in => f =>
        in |> map(f(0)) >> map(f(1) >> f(2) >> f(3))),
      Seq(rules.eta, rules.betaExtract, rules.mapFission, rules.mapFusion)
    )
  }

  test("map first fission (CNF)") {
    ProveEquiv.init().runCNF(
      withArrayAndFuns(4, in => f =>
        in |> map(f(0) >> f(1) >> f(2) >> f(3))),
      withArrayAndFuns(4, in => f =>
        in |> map(f(0)) >> map(f(1) >> f(2) >> f(3))),
      Seq(rules.combinatory.compositionAssoc1,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.mapFission,
        rules.combinatory.mapFusion)
    )
  }

  test("slideBeforeMapMapF") {
    val `__` = rct.TypePlaceholder
    def wrap(inner: ToBeTyped[rc.Expr] => ToBeTyped[rc.Expr])
    : rc.Expr =
      depFun((n: rct.Nat) =>
      depFun((dt1: rct.DataType) => depFun((dt2: rct.DataType) =>
      fun(f =>
        inner(f :: dt1 ->: dt2) :: ((n`.`dt1) ->: `__`)
      ))))

    ProveEquiv.init().runBENF(
      wrap(f =>
        slide(3)(1) >> slide(4)(2) >> map(map(map(f)))),
      wrap(f =>
        map(f) >> slide(3)(1) >> slide(4)(2)),
      Seq(rules.eta, rules.betaExtract, rules.mapFusion, rules.mapFission, rules.slideBeforeMapMapF)
    )
  }

  test("composition associativity") {
    ProveEquiv.init().runCNF(
      withArrayAndFuns(4, in => f =>
        in |> map(((f(0) >> f(1)) >> f(2)) >> f(3))),
      withArrayAndFuns(4, in => f =>
        in |> map(f(0) >> (f(1) >> (f(2) >> f(3))))),
      Seq())
  }

  ignore("saturate associativity and fusion/fission") {
    withFuns(4)
    withFuns(5)
    withFuns(6)
    withFuns(7)
    withFuns(9)
    withFuns(10)

    def withFuns(n: Int): Unit = {
      try {
        ProveEquiv.init().runCNF(
          withArrayAndFuns(n, in => f =>
            in |> map(f.reduce(_>>_))),
          // unreachable by design
          withArrayAndFuns(n, in => f =>
            in |> map(f.reduce(_>>_)) >> split(2) >> join),
          Seq(
            rules.combinatory.mapFusion,
            rules.combinatory.mapFission,
            rules.combinatory.compositionAssoc1,
            rules.combinatory.compositionAssoc2))
      } catch {
        case CouldNotProveEquiv =>
      }
    }
  }
}

object Basic {
  import rise.core.DSL._
  import rise.core.DSL.Type._

  def introduceDataFuns(n: Int,
                        k: Seq[rct.DataType] => Seq[ToBeTyped[rc.Identifier]] => ToBeTyped[rc.Expr]
                       ): ToBeTyped[rc.Expr] = {
    def recDataTypes(n: Int,
                     k: Seq[rct.DataType] => ToBeTyped[rc.Expr]
                    ): ToBeTyped[rc.Expr] =
      if (n <= 0) {
        k(Seq())
      } else {
        recDataTypes(n - 1, rest => depFun((dt: rct.DataType) => k(dt +: rest)))
      }

    def recFuns(n: Int, dts: Seq[rct.DataType],
                k: Seq[ToBeTyped[rc.Identifier]] => ToBeTyped[rc.Expr]
               ): ToBeTyped[rc.Expr] =
      if (n <= 0) {
        k(Seq())
      } else {
        recFuns(n - 1, dts, rest => fun((dts(n) ->: dts(n - 1)): rct.ExprType)(f => k(f +: rest)))
      }

    recDataTypes(n + 1, dts => recFuns(n, dts, k(dts)))
  }

  def withArrayAndFuns(n: Int,
                       k: ToBeTyped[rc.Expr] => Seq[ToBeTyped[rc.Expr]] => ToBeTyped[rc.Expr]
                      ): rc.Expr = {
    impl { elemT: rct.DataType =>
      depFun((size: rct.Nat) => introduceDataFuns(n, _ => f => fun(size`.`elemT)(in => k(in)(f)))) }
  }
}
