package elevate.core

import elevate.lift.rules.algorithmic.{mapFusion}
import elevate.lift._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import lift.core.DSL._
import lift.core.primitives.map

class traversals extends idealised.util.Tests {

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
      ).forall(x => structEq(x.get, expr))
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
      ).forall(x => structEq(x.get, gold))
    )
  }
}
