package rise.eqsat

class ExprCheck extends test_util.Tests {
  /* FIXME
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

  test("withNatArgument") {
    import ExprDSL._

    // (Λn. Λm. transpose: n.m.dt -> m.n.dt) o 1
    // (Λ. Λ. transpose: %n1.%n0.%dt0 -> %n0.%n1.%dt0) %n0
    // -->
    // transpose: o.1.dt -> 1.o.dt
    // transpose: %n0.1.%dt0 -> 1.%n0.%dt0
    assert((
      nLam(transpose(
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0)))
      )) withNatArgument
        `%n`(0))
      ==
      nLam(transpose(
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0)))
      ))
    )
    assert((
      transpose(
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0)))
      ) withNatArgument
        cst(1))
      ==
      transpose(
        (`%n`(0)`.`(cst(1)`.``%dt`(0))) ->:
        (cst(1)`.`(`%n`(0)`.``%dt`(0)))
      )
    )

    // Λn. ((Λm. transpose: n.m.dt -> m.n.dt) o) 1
    // Λ. ((Λ. transpose: %n1.%n0.%dt0 -> %n0.%n1.%dt0) %n1) 1
    // -->
    // transpose: 1.o.dt -> o.1.dt
    // transpose: 1.%n0.%dt0 -> %n0.1.%dt0
    assert((
      transpose(
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0)))
      ) withNatArgument
        `%n`(1))
      ==
      transpose(
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0))) ->:
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0)))
      )
    )
    assert((
      transpose(
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0))) ->:
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0)))
      ) withNatArgument
        cst(1))
      ==
      transpose(
        (cst(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(cst(1)`.``%dt`(0)))
      )
    )
    assert((
      nApp(nLam(transpose(
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0)))
      )), `%n`(1),
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0))) ->:
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0)))
      ) withNatArgument
        cst(1))
      ==
      nApp(nLam(transpose(
        (cst(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(cst(1)`.``%dt`(0)))
      )), `%n`(0),
        (cst(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(cst(1)`.``%dt`(0)))
      )
    )
    assert((
      transpose(
        (cst(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(cst(1)`.``%dt`(0)))
      ) withNatArgument
        `%n`(0))
      ==
      transpose(
        (cst(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(cst(1)`.``%dt`(0)))
      )
    )
  }
*/
  test("fromNamed") {
    val named: Seq[rise.core.Expr] = {
      import rise.core.types._
      import rise.core.primitives._
      import rise.core.DSL._
      import rise.core.DSL.Type._

      Seq(
        depFun((n: Nat) => depFun((m: Nat) => fun(
          (n`.`m`.`f32) ->: (m`.`n`.`f32)
        )(input => input |>
          transpose
        ))),
        depFun((n: Nat) => depFun((m: Nat) => fun(
          ((n+2)`.`f32) ->: (n`.`f32)
        )(input => input |>
          take(n)
        )))
      )
    }

    val expectedDebruijn: Seq[Expr] = {
      import ExprDSL._

      Seq(
        nLam(nLam(lam(`%n`(1) `.` (`%n`(0) `.` f32),
          app(transpose((`%n`(1) `.` (`%n`(0) `.` f32)) ->: (`%n`(0) `.` (`%n`(1) `.` f32))),
            %(0, `%n`(1) `.` (`%n`(0) `.` f32)))
        ))),
        nLam(nLam(lam(Nat(NatAdd(`%n`(1), cst(2)))`.`f32,
          app(nApp(Expr(Primitive(rise.core.primitives.take.primitive),
            nFunT((Nat(NatAdd(`%n`(0), cst(2)))`.`f32) ->: (`%n`(0)`.`f32))),
           `%n`(1), (Nat(NatAdd(`%n`(1), cst(2)))`.`f32) ->: (`%n`(1)`.`f32)),
            %(0, Nat(NatAdd(`%n`(1), cst(2)))`.`f32))
        )))
      )
    }

    for ((n, ed) <- named.zip(expectedDebruijn)) {
      val debruijn = Expr.fromNamed(n)
      assert(ed == debruijn)
      val backToNamed = Expr.toNamed(debruijn)
      assert(n =~= backToNamed)
    }
  }
}
