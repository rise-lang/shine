package rise.eqsat

class ExprCheck extends test_util.Tests {
  test("withArgument") {
    import ExprDSL._

    // (λ. (λ. ((λ. (0 1)) (0 1)))) --> (λ. (λ. ((0 1) 0)))
    // (λ. (0 1)) (0 1) --> (0 1) 0
    assert((
      app(%(0, f32 ->: f32), %(1, f32))
        withArgument
      app(%(0, f32 ->: f32 ->: f32), %(1, f32)))
        ==
      app(app(%(0, f32 ->: f32 ->: f32), %(1, f32)),
        %(0, f32)))
    // r1 = (app (lam (app "%6" (app "%5" "%0"))) "%0")
    // r2 = (app (lam (app "%6" r1)) "%0")
    // r3 = (app (lam (app "%6" r2)) %0)
    // (app map (lam (app "%6" r3)))
    // --> (app map (lam (app "%6" (app "%5" (app "%4" (app "%3" (app "%2" "%0")))))))
    assert((
      app(%(6, f32 ->: f32), app(%(5, f32 ->: f32), %(0, f32)))
        withArgument
      %(0, f32))
        ==
      app(%(5, f32 ->: f32), app(%(4, f32 ->: f32), %(0, f32))))
  }

  test("fromNamed") {
    val named: rise.core.Expr = {
      import rise.core.types._
      import rise.core.primitives._
      import rise.core.DSL._
      import rise.core.DSL.Type._

      depFun((n: Nat) => depFun((m: Nat) => fun(
        (n`.`m`.`f32) ->: (m`.`n`.`f32)
      )(input => input |>
        transpose
      )))
    }

    val expectedDebruijn = {
      import ExprDSL._

      nLam(nLam(lam(`%n`(1)`.`(`%n`(0)`.`f32),
        app(transpose((`%n`(1)`.`(`%n`(0)`.`f32)) ->: (`%n`(0)`.`(`%n`(1)`.`f32))),
          %(0, `%n`(1)`.`(`%n`(0)`.`f32)))
      )))
    }

    val debruijn = Expr.fromNamed(named)
    assert(expectedDebruijn == debruijn)
    val backToNamed = Expr.toNamed(debruijn)
    assert(named =~= backToNamed)
  }
}
