package rise.eqsat

import rise.core.Expr
import rise.core.primitives._
import rise.core.types._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.eqsat.ExprSet
import rise.eqsat.rules._
import rise.eqsat.strategies._

class basic extends test_util.Tests {
  private val BENF = rise.elevate.strategies.normalForm.BENF()(
    rise.elevate.rules.traversal.alternative.RiseTraversable)
  val additiveBetaEta = any(
    topDown(betaReduction),
    topDown(etaReduction)
  )
  val destructiveBetaEta = any(
    topDown(betaReduction, destructive = true),
    topDown(etaReduction, destructive = true)
  )

  val n = NatIdentifier("N", isExplicit = true)
  val dt = DataTypeIdentifier("dt", isExplicit = true)
  val ft: Type = dt ->: dt
  val f1: ToBeTyped[Expr] = rise.core.Identifier("f1")(ft)
  val f2: ToBeTyped[Expr] = rise.core.Identifier("f2")(ft)
  val f3: ToBeTyped[Expr] = rise.core.Identifier("f3")(ft)
  val f4: ToBeTyped[Expr] = rise.core.Identifier("f4")(ft)

  test("init represents itself") {
    val e: Expr = fun(n`.`dt)(x => x |> map(f1) >> map(f2))
    val set = ExprSet.init(e)
    assert(set.represents(e))
  }

  test("init represents equivalent") {
    val a: Expr = fun(n`.`dt)(x => x |> map(f1) >> map(f2))
    val b: Expr = fun(n`.`dt)(x => x |> map(f1) >> map(f2))
    val set = ExprSet.init(a)
    assert(set.represents(b))
  }

  test("BENF") {
    val e: Expr = fun(n`.`dt)(x => x |> map(f1) >> map(f2))
    val benf: Expr = BENF(e).get
    val additiveSet = ExprSet.init(e)
    normalize(additiveBetaEta)(additiveSet)
    assert(additiveSet.represents(benf))
    val destructiveSet = ExprSet.init(e)
    normalize(destructiveBetaEta)(destructiveSet)
    assert(destructiveSet.represents(benf))
  }

  test("mapFusion") {
    val start: Expr = fun(n`.`dt)(x =>
      x |> map(f1) >> map(f2) >> map(f3) >> map(f4)
    )
    val goal: Expr = fun(n`.`dt)(x =>
      x |> map(f1 >> f2) >> map(f3 >> f4)
    )
    val set = ExprSet.init(start)
    normalize(seq(
      // TODO: how can we use destructiveBetaEta here?
      normalize(additiveBetaEta),
      topDown(mapFusion)
    ))(set)
    assert(set.represents(BENF(goal).get))
  }

  test("mapFission") {
    val start: Expr = fun(n`.`dt)(x =>
      x |> map(f1 >> f2 >> f3)
    )
    val goal: Expr = BENF(fun(n`.`dt)(x =>
      x |> map(f1) >> map(f2) >> map(f3)
    )).get
    val set1 = ExprSet.init(start)
    repeat(4, seq(
      normalize(additiveBetaEta),
      topDown(mapFission),
    ))(set1)
    //util.dotPrintTmp("set-end-", set1)
    //util.dotPrintTmp("goal", goal)
    assert(set1.represents(goal))/*
    val set2 = ExprSet.init(start)
    // FIXME: can't normalize because of destructive rule
    repeat(2, seq(
      normalize(destructiveBetaEta),
      topDown(mapFission)
    ))(set2)
    assert(set2.represents(goal))*/
  }

  test("mapFission + mapFusion") {
    val start: Expr = fun(n`.`dt)(x =>
      x |> map(f1 >> f2 >> f3 >> f4)
    )
    val goal: Expr = fun(n`.`dt)(x =>
      x |> map(f1 >> f2) >> map(f2 >> f3)
    )
    val set = ExprSet.init(start)
    // FIXME: can't normalize because of destructive rule
    repeat(3, seq(
      normalize(destructiveBetaEta),
      topDown(mapFission),
      normalize(destructiveBetaEta),
      topDown(mapFusion),
      normalize(destructiveBetaEta),
      dotPrintTmp("set")
    ))(set)
    assert(set.represents(BENF(goal).get))
  }
}
