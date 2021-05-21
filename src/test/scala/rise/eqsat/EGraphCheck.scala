package rise.eqsat

import rise.core.{primitives => rcp}
import ExprDSL._

class EGraphCheck extends test_util.Tests {
  test("simple add") {
    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)
    val x = egraph.add(Var(0), egraph.addDataType(f32))
    val x2 = egraph.add(Var(0), egraph.addDataType(f32))
    egraph.add(Var(0), egraph.addDataType(cst(1)`.`f32))
    val add = egraph.add(Primitive(rcp.add.primitive), egraph.addType(f32 ->: f32 ->: f32))
    val addx = egraph.add(App(add, x), egraph.addType(f32 ->: f32))
    egraph.add(App(addx, x2), egraph.addDataType(f32))

    val y = egraph.add(Var(1), egraph.addDataType(f32))
    egraph.union(x, y)
    egraph.rebuild()

    egraph.dot().toFile("/tmp/egraph-simple-add.dot")
  }

  test("canonical nats") {
    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)
    val x = egraph.add(Var(0), egraph.addType(Type(IndexType(Nat(
      NatAdd(Nat(NatVar(0)), Nat(NatAdd(Nat(NatVar(2)), Nat(NatVar(1))))))))))
    val x2 = egraph.add(Var(0), egraph.addType(Type(IndexType(Nat(
      NatAdd(Nat(NatVar(1)), Nat(NatAdd(Nat(NatVar(0)), Nat(NatVar(2))))))))))
    assert(x == x2)
  }
/* FIXME
  test("EClass withArgument") {
    import ExprDSL._

    def check(body: Expr, arg: Expr): Unit = {
      val egraph = EGraph.emptyWithAnalysis(DefaultAnalysis)
      val b = egraph.addExpr(body)
      val a = egraph.addExpr(arg)
      val r = EClass.withArgument(b, a, egraph)
      val result = Extractor.init(egraph, AstSize).findBestOf(r)._2
      assert(result == (body withArgument arg))
    }

    check(
      app(%(0, f32 ->: f32), %(1, f32)),
      app(%(0, f32 ->: f32 ->: f32), %(1, f32))
    )
    check(
      app(%(6, f32 ->: f32), app(%(5, f32 ->: f32), %(0, f32))),
      %(0, f32)
    )
  }

  test("EClass withNatArgument") {
    import ExprDSL._

    def check(body: Expr, arg: Nat): Unit = {
      val egraph = EGraph.emptyWithAnalysis(DefaultAnalysis)
      val b = egraph.addExpr(body)
      val r = EClass.withNatArgument(b, arg, egraph)
      val result = Extractor.init(egraph, AstSize).findBestOf(r)._2
      assert(result == (body withNatArgument arg))
    }

    check(
      nLam(transpose(
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0)))
      )),
      `%n`(0)
    )
    check(
      transpose(
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0)))
      ),
      cst(1)
    )

    check(
      transpose(
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0)))
      ),
      `%n`(1)
    )
    check(
      transpose(
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0))) ->:
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0)))
      ),
      cst(1)
    )
    check(
      nApp(nLam(transpose(
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0)))
      )), `%n`(1),
        (`%n`(0)`.`(`%n`(1)`.``%dt`(0))) ->:
        (`%n`(1)`.`(`%n`(0)`.``%dt`(0)))
      ),
      cst(1)
    )
    check(
      transpose(
        (cst(1)`.`(`%n`(0)`.``%dt`(0))) ->:
        (`%n`(0)`.`(cst(1)`.``%dt`(0)))
      ),
      `%n`(0)
    )
  }

 */
}
