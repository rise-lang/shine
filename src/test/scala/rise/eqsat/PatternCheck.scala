package rise.eqsat

class PatternCheck extends test_util.Tests {
  test("simple match") {
    val commuteAdd1: DefaultAnalysis.Rewrite = {
      import PatternDSL._
      Rewrite.init("commute-add-1",
        app(app(add, ?(0) :: `?dt`(0)), ?(1)).compile()
          -->
        (app(app(add :: `?dt`(0) ->: `?dt`(0) ->: `?dt`(0),
          ?(1) :: `?dt`(0)) :: `?dt`(0) ->: `?dt`(0),
          ?(0) :: `?dt`(0)) :: `?dt`(0)))
    }

    val commuteAdd2: DefaultAnalysis.Rewrite = {
      import NamedRewriteDSL._
      NamedRewrite.init("commute-add-2",
        app(app(add, "x"), "y")
          -->
        app(app(add, "y"), "x"))
    }

    for (commuteAdd <- Seq(commuteAdd1, commuteAdd2)) {
      val egraph = EGraph.emptyWithAnalysis(DefaultAnalysis)

      val (add1, add2) = {
        import ExprDSL._
        (egraph.addExpr(app(app(add(f32 ->: f32 ->: f32), %(0, f32)), %(1, f32))),
          egraph.addExpr(app(app(add(f32 ->: f32 ->: f32), %(2, f32)), %(3, f32))))
      }

      egraph.union(add1, add2)
      egraph.rebuild()
      val shc = SubstHashCons.empty
      val matches = commuteAdd.search(egraph, shc)
      val nMatches = matches.map(m => m.substs.size).sum
      assert(nMatches == 2)

      val applications = commuteAdd.apply(egraph, shc, matches)
      egraph.rebuild()
      assert(applications.size == 2)

      egraph.dot().toFile(s"/tmp/simple-${commuteAdd.name}.dot")
    }
  }

  test("compile program with depApps") {
    import PatternDSL._

    val x = `?n`(0)
    val pattern = nApp(nApp(add, x), x).compile()

    import ematching._
    assert(
      pattern.prog.instructions == Vec(
        PushType(Reg(0)),
        Bind(NatApp((), ()), Reg(0), Reg(1), NatReg(0), TypeReg(1)),
        PushType(Reg(1)),
        Bind(NatApp((), ()), Reg(1), Reg(2), NatReg(1), TypeReg(2)),
        PushType(Reg(2)),
        Bind(Primitive(rise.core.primitives.add.primitive), Reg(2), Reg(3), NatReg(2), TypeReg(3)),
        // TODO: the bind below will be executed on each match backtracking,
        //  it is probably best to reorder execution in a smarter way
        NatCompare(NatReg(0), NatReg(1))
      )
    )
    assert(pattern.prog.v2r == HashMap())
    assert(pattern.prog.n2r == HashMap(x -> NatReg(1)))

    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)

    {
      import ExprDSL._
      egraph.addExpr(nApp(nApp(add(nFunT(nFunT(f32))), `%n`(0), nFunT(f32)), cst(1), f32))
      egraph.addExpr(nApp(nApp(add(nFunT(nFunT(f32))), `%n`(0), nFunT(f32)), `%n`(1), f32))
      egraph.addExpr(nApp(nApp(add(nFunT(nFunT(f32))), `%n`(0), nFunT(f32)), `%n`(0), f32))
      egraph.addExpr(nApp(nApp(mul(nFunT(nFunT(f32))), `%n`(0), nFunT(f32)), `%n`(0), f32))
    }

    egraph.rebuild()

    val shc = SubstHashCons.empty
    assert(pattern.search(egraph, shc).length == 1)
  }

  test("compile program with types") {
    import PatternDSL._

    val pattern = (map :: (`?t` ->: `?dt` ->: (`?n`(0)`.``?dt`(0)))).compile()

    import ematching._
    assert(
      pattern.prog.instructions == Vec(
        PushType(Reg(0)),
        Bind(Primitive(rise.core.primitives.map.primitive), Reg(0), Reg(1), NatReg(0), TypeReg(1)),
        // TODO: the bind below will be executed on each match backtracking,
        //  it is probably best to reorder execution in a smarter way
        TypeBind(FunType((), ()), TypeReg(0)),
        TypeBind(FunType((), ()), TypeReg(2)),
        TypeBind(ArrayType((), ()), TypeReg(4)),
        // TODO: the check below could be eliminated since we know that this is an array element
        DataTypeCheck(TypeReg(5)),
        DataTypeCheck(TypeReg(3))
      )
    )
    assert(pattern.prog.v2r == HashMap())
    assert(pattern.prog.n2r == HashMap(`?n`(0) -> NatReg(0)))
    assert(pattern.prog.dt2r == HashMap(`?dt`(0) -> TypeReg(5)))

    val egraph = EGraph.emptyWithAnalysis(NoAnalysis)

    {
      import ExprDSL._
      egraph.addExpr(map((f32 ->: int) ->: (`%n`(0)`.`f32) ->: (`%n`(0)`.`int)))
      egraph.addExpr(map((f32 ->: `%dt`(0)) ->: (`%n`(0)`.`f32) ->: (cst(0)`.``%dt`(0))))
      egraph.addExpr(map((f32 ->: int) ->: (f32 ->: int) ->: (`%n`(0)`.`int)))
      egraph.addExpr(map((f32 ->: int) ->: (f32 ->: int)))
      egraph.addExpr(map((f32 ->: int) ->: f32))
      egraph.addExpr(map(int))
    }

    egraph.rebuild()

    val shc = SubstHashCons.empty
    assert(pattern.search(egraph, shc).length == 2)
  }
}
