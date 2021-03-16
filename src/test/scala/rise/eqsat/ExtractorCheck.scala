package rise.eqsat

import rise.core.{primitives => rcp}

class ExtractorCheck extends test_util.Tests {
  test("simple extraction") {
    import rise.debruijn._
    import rise.core.semantics._

    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)
    val expr = egraph.addExpr({
      val two = Literal(IntData(2))
      val mulTwo = App(App(Primitive(rcp.mul.primitive), Var(0)), two)
      App(App(Primitive(rcp.div.primitive), mulTwo), two)
    })
    val simpler = egraph.addExpr(Var(0))
    egraph.union(expr, simpler)
    egraph.rebuild()

    val extractor = Extractor.init(egraph, AstSize)
    assert(extractor.findBestOf(expr) == (1, Var(0)))
  }
}
