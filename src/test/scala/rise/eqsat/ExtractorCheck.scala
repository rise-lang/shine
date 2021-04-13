package rise.eqsat

import rise.core.semantics._
import ExprDSL._

class ExtractorCheck extends test_util.Tests {
  test("simple extraction") {
    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)
    val expr = egraph.addExpr({
      val two = l(IntData(2))
      val mulTwo = app(app(mul, %(0)), two)
      app(app(div, mulTwo), two)
    })
    val simpler = egraph.addExpr(%(0))
    egraph.union(expr, simpler)
    egraph.rebuild()

    val extractor = Extractor.init(egraph, AstSize)
    assert(extractor.findBestOf(expr) == (1, %(0)))
  }
}
