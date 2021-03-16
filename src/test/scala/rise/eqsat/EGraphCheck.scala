package rise.eqsat

import rise.core.{primitives => rcp}

class EGraphCheck extends test_util.Tests {
  test("simple add") {
    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)
    val x = egraph.add(Var(0))
    val x2 = egraph.add(Var(0))
    val add = egraph.add(Primitive(rcp.add.primitive))
    val addx = egraph.add(App(add, x))
    egraph.add(App(addx, x2))

    val y = egraph.add(Var(1))
    egraph.union(x, y)
    egraph.rebuild()

    dot.toSVG(egraph, "/tmp/egraph-simple-add.svg")
  }
}
