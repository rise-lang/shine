package rise.eqsat

import ExprDSL._

class ExtractorCheck extends test_util.Tests {
  test("simple extraction") {
    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)
    val expr = egraph.addExpr({
      import rise.core.DSL._
      Expr.fromNamed(fun(x => (x * l(2)) / l(2)))
    })
    val simplerExpr = lam(int, %(0, int))
    val simpler = egraph.addExpr(simplerExpr)
    egraph.union(expr, simpler)
    egraph.rebuild()

    val extractor = Extractor.init(egraph, AstSize)
    assert(extractor.findBestOf(expr) == (2, simplerExpr))
  }
}
