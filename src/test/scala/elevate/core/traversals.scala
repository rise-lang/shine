package elevate.core

import elevate.core.rules._
import elevate.core.rules.algorithmic.{mapFusion, mapLastFission}
import elevate.core.strategies._
import elevate.core.strategies.algorithmic.{mapFirstFission, mapFullFission}
import elevate.core.strategies.traversal._
import lift.core.DSL._
import lift.core._
import lift.core.primitives._

class traversals extends idealised.util.Tests {
  val norm = normalize(betaReduction <+ etaReduction)

  def eq(a: Expr, b: Expr): Boolean = StructuralEquality(norm(a), norm(b))

  test("id traversals") {
    val expr = fun(f => fun(g => map(f) >> map(g)))

    val rewritten1 = topdown(id)(expr)
    val rewritten2 = bottomup(id)(expr)
    val rewritten3 = downup(id)(expr)
    val rewritten4 = downup2(id)(id)(expr)
    val rewritten5 = oncetd(id)(expr)
    val rewritten6 = oncebu(id)(expr)
    val rewritten7 = alltd(id)(expr)
    val rewritten8 = sometd(id)(expr)
    val rewritten9 = somebu(id)(expr)

    assert(eq(rewritten1, expr))
    assert(eq(rewritten2, expr))
    assert(eq(rewritten3, expr))
    assert(eq(rewritten4, expr))
    assert(eq(rewritten5, expr))
    assert(eq(rewritten6, expr))
    assert(eq(rewritten7, expr))
    assert(eq(rewritten8, expr))
    assert(eq(rewritten9, expr))
  }

  test("simple fusion") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val gold = fun(f => fun(g => map(f >> g)))

    val rewritten1 = oncetd(mapFusion)(expr)
    val rewritten2 = oncebu(mapFusion)(expr)
    val rewritten3 = alltd(mapFusion)(expr)
    val rewritten4 = sometd(mapFusion)(expr)
    val rewritten5 = somebu(mapFusion)(expr)

    val rewritten6 = topdown(`try`(mapFusion))(expr)
    val rewritten7 = bottomup(`try`(mapFusion))(expr)

    assert(eq(rewritten1, gold), s"\n$rewritten1 !=\n$gold")
    assert(eq(rewritten2, gold), s"\n$rewritten2 !=\n$gold")
    assert(eq(rewritten3, gold), s"\n$rewritten3 !=\n$gold")
    assert(eq(rewritten4, gold), s"\n$rewritten4 !=\n$gold")
    assert(eq(rewritten5, gold), s"\n$rewritten5 !=\n$gold")
    assert(eq(rewritten6, gold), s"\n$rewritten6 !=\n$gold")
    assert(eq(rewritten7, gold), s"\n$rewritten7 !=\n$gold")
  }
}
