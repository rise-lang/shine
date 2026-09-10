package rise.eqsat

import ExprDSL._

class ExtractorCheck extends test_util.Tests {
  test("simple extraction") {
    val egraph = EGraph.empty()
    val expr = egraph.addExpr({
      import rise.core.DSL._
      Expr.fromNamed(fun(x => (x * l(2)) / l(2)))
    })
    val simplerExpr = lam(int, %(0, int))
    val simpler = egraph.addExpr(simplerExpr)
    egraph.union(expr, simpler)
    egraph.rebuild(Seq(expr))

    val (bestExpr, bestSize) = Extractor.findBestOf(egraph, AstSize, expr)
    assert((bestSize, ExprWithHashCons.expr(egraph)(bestExpr)) == (2, simplerExpr))
    assert(AstSize.ofExpr(simplerExpr) == 2)
  }

  test("simple named AstSize") {
    import rise.core.DSL._

    def sz(e: ToBeTyped[rise.core.Expr]): Int =
      AstSize.ofNamedExpr(e.toUntypedExpr)

    assert(sz(fun(a => a)) == 2)
    assert(sz(fun(a => a * l(4))(l(1) + l(2))) == 12)
  }
}
