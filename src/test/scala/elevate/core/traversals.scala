package elevate.core

import elevate.lift.rules._
import elevate.util._
import elevate.lift.strategies.normalForm._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.traversal._
import elevate.lift.rules
import elevate.lift.rules.algorithmic._
import elevate.lift.strategies.traversal.{body, function, inBody}
import elevate.meta.rules.traversal.bodyFission
import lift.core.DSL._
import lift.core.Expr
import lift.core.primitives.map


class traversals extends idealised.util.Tests {

  test("simple") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val strategy = body(body(body(mapFusion `;` function(mapLastFission))))

    val metaStrategy = inBody(inBody(bodyFission))(strategy)
    val newStrategy = metaStrategy.get
    println("------------------")
    println(strategy)
    println(newStrategy)
    println("------------------")
    println(newStrategy(expr))
    println(strategy(expr))
  }

  test("id traversals") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val id = strategies.basic.id[Lift]()

    assert(
      List(
        topdown(id).apply(expr),
        bottomup(id).apply(expr),
        downup(id).apply(expr),
        downup2(id,id).apply(expr),
        oncetd(id).apply(expr)
        //oncebu(id)(expr),
        //alltd(id)(expr),
        //sometd(id)(expr),
        //innermost(id)(expr),
        //somebu(id)(expr)
      ).forall(x => betaEtaEquals(x.get, expr))
    )
  }

  /*
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
      ).forall(x => betaEtaEquals(x.get, gold))
    )

  }
   */
}
