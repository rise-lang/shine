package elevate.core

import elevate.core.rules._
import elevate.core.rules.algorithmic.{mapFusion}
import elevate.core.strategies._
import elevate.core.strategies.traversal._
import elevate.core.strategies.normalForm._
import elevate.core.strategies.basic._
import lift.core.DSL._
import lift.core._
import lift.core.primitives._

class traversals extends idealised.util.Tests {
  val norm = normalize(betaReduction <+ etaReduction)

  def eq(a: Expr, b: Expr): Boolean = StructuralEquality(norm(a).get, norm(b).get)

  test("id traversals") {
    val expr = fun(f => fun(g => map(f) >> map(g)))

    assert(
      List(
        topdown(id)(expr),
        bottomup(id)(expr),
        downup(id)(expr),
        downup2(id)(id)(expr),
        oncetd(id)(expr),
        oncebu(id)(expr),
        alltd(id)(expr),
        sometd(id)(expr),
        //innermost(id)(expr),
        somebu(id)(expr)
      ).forall(x => eq(x.get, expr))
    )
  }

  test("simple fusion") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val gold = fun(f => fun(g => map(f >> g)))

    assert(
      List(
        oncetd(mapFusion)(expr),
        oncebu(mapFusion)(expr),
        alltd(mapFusion)(expr),
        sometd(mapFusion)(expr),
        somebu(mapFusion)(expr),
        topdown(`try`(mapFusion))(expr),
        bottomup(`try`(mapFusion))(expr)
      ).forall(x => eq(x.get, gold))
    )
  }
}
