package rise.eqsat

import rise.core.{primitives => rcp}
import ExprDSL._

class EGraphCheck extends test_util.Tests {
  test("simple add") {
    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)
    val x = egraph.add(Var(0), f32)
    val x2 = egraph.add(Var(0), f32)
    egraph.add(Var(0), cst(1)`.`f32)
    val add = egraph.add(Primitive(rcp.add.primitive), f32 ->: f32 ->: f32)
    val addx = egraph.add(App(add, x), f32 ->: f32)
    egraph.add(App(addx, x2), f32)

    val y = egraph.add(Var(1), f32)
    egraph.union(x, y)
    egraph.rebuild()

    egraph.dot().toFile("/tmp/egraph-simple-add.dot")
  }
}
