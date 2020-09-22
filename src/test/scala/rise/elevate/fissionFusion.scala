package rise.elevate

import elevate.core.Strategy
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal.{position, topDown}
import rise.elevate.util._
import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core._
import rise.elevate.rules.algorithmic.{mapFusion, mapLastFission}
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.{mapFirstFission, mapFullFission}


class fissionFusion extends test_util.Tests {

  val BENF = rise.elevate.strategies.normalForm.BENF()(RiseTraversable)

  def eq(a: Expr, b: Expr): Unit = {
    if (BENF(a).get != BENF(b).get) {
      throw new Exception(s"expected structural equality:\n$a\n$b")
    }
  }

  def check(a: Expr, fis: Strategy[Rise], b: Expr, fus: Strategy[Rise]): Unit = {
    val (closedA, nA) = makeClosed(a)
    val (closedB, nB) = makeClosed(b)
    val na = BENF(closedA).get
    val nb = BENF(closedB).get
    eq(position(nA)(fis).apply(na).get, nb)
    eq(position(nB)(fus).apply(nb).get, na)
  }

  test("last fission, chain of 2") {
    check(
      fun(f1 => fun(f2 => map(f1 >> f2))),
      position(2)(mapLastFission()),
      fun(f1 => fun(f2 => map(f1) >> map(f2))),
      topDown(mapFusion))
  }

  test("last fission, chain of 3") {
    check(
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2 >> f3)))),
      position(3)(mapLastFission()),
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2) >> map(f3)))),
      topDown(mapFusion))
  }

  test("first fission, chain of 2") {
    check(
      fun(f1 => fun(f2 => map(f1 >> f2))),
      position(2)(mapFirstFission),
      fun(f1 => fun(f2 => map(f1) >> map(f2))),
      topDown(mapFusion))
  }

  test("first fission, chain of 3") {
    check(
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2 >> f3)))),
      position(3)(mapFirstFission),
      fun(f1 => fun(f2 => fun(f3 => map(f1) >> map(f2 >> f3)))),
      topDown(mapFusion))
  }

  test("full fission, chain of 2") {
    check(
      fun(f1 => fun(f2 => map(f1 >> f2))),
      position(2)(mapFullFission),
      fun(f1 => fun(f2 => map(f1) >> map(f2))),
      topDown(mapFusion))
  }

  test("full fission, chain of 3") {
    check(
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2 >> f3)))),
      position(3)(mapFullFission),
      fun(f1 => fun(f2 => fun(f3 => map(f1) >> map(f2) >> map(f3)))),
      normalize(RiseTraversable)(mapFusion))
  }
}
