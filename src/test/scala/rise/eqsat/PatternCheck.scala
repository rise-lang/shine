package rise.eqsat

class PatternCheck extends test_util.Tests {
  test("simple match") {
    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)

    val (add1, add2) = {
      import ExprDSL._
      (egraph.addExpr(app(app(add, %(0)), %(1))),
       egraph.addExpr(app(app(add, %(2)), %(3))))
    }

    egraph.union(add1, add2)
    egraph.rebuild()

    val commuteAdd = {
      import PatternDSL._
      Rewrite.init[()]("commute-add",
        app(app(add, ?("a")), ?("b")).compile(),
        app(app(add, ?("b")), ?("a")).compile())
    }

    val matches = commuteAdd.search(egraph)
    val nMatches = matches.map(m => m.substs.size).sum
    assert(nMatches == 2)

    val applications = commuteAdd.apply(egraph, matches)
    egraph.rebuild()
    assert(applications.size == 2)

    egraph.dot().toSVG("/tmp/simple-match.svg")
  }
}
