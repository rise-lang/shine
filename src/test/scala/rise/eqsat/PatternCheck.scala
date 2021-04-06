package rise.eqsat

class PatternCheck extends test_util.Tests {
  test("simple match") {
    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)

    val (add1, add2) = {
      import ExprDSL._
      (egraph.addExpr(app(app(add(f32 ->: f32 ->: f32), %(0, f32)), %(1, f32))),
       egraph.addExpr(app(app(add(f32 ->: f32 ->: f32), %(2, f32)), %(3, f32))))
    }

    egraph.union(add1, add2)
    egraph.rebuild()

    val commuteAdd: Rewrite[()] = ???/*{
      import PatternDSL._
      Rewrite.init[()]("commute-add",
        app(app(add, ?("a")), ?("b")).compile(),
        app(app(add, ?("b")), ?("a")).compile())
    }*/

    val matches = commuteAdd.search(egraph)
    val nMatches = matches.map(m => m.substs.size).sum
    assert(nMatches == 2)

    val applications = commuteAdd.apply(egraph, matches)
    egraph.rebuild()
    assert(applications.size == 2)

    egraph.dot().toFile("/tmp/simple-match.dot")
  }

  test("compile program with depApps") {
    import PatternDSL._

    val x = `?n`(0)
    val pattern = (nApp(nApp(add :: `?t`(0), x) :: `?t`(1), x) :: `?t`(2)).compile()

    import ematching._
    assert(
      pattern.prog.instructions == Vec(
        Bind(NatApp((), ()), Reg(0), Reg(1), NatReg(0)),
        Bind(NatApp((), ()), Reg(1), Reg(2), NatReg(1)),
        NatCompare(NatReg(1), NatReg(0)),
        Bind(Primitive(rise.core.primitives.add.primitive), Reg(2), Reg(3), NatReg(2))
      )
    )
    assert(
      pattern.prog.v2r == HashMap()
    )

    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)

    {
      import ExprDSL._
      egraph.addExpr(nApp(nApp(add(nFunT(nFunT(f32))), `%n`(0), nFunT(f32)), cst(1), f32))
      egraph.addExpr(nApp(nApp(add(nFunT(nFunT(f32))), `%n`(0), nFunT(f32)), `%n`(1), f32))
      egraph.addExpr(nApp(nApp(add(nFunT(nFunT(f32))), `%n`(0), nFunT(f32)), `%n`(0), f32))
      egraph.addExpr(nApp(nApp(mul(nFunT(nFunT(f32))), `%n`(0), nFunT(f32)), `%n`(0), f32))
    }

    egraph.rebuild()

    assert(pattern.search(egraph).length == 1)
  }
}
