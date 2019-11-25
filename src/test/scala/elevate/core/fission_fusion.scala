package elevate.core

import lift.core._
import lift.core.DSL._
import lift.core.primitives.Map
import elevate.core.strategies.basic._
import elevate.lift.rules._
import elevate.lift.rules.algorithmic.{mapFusion, mapLastFission}
import elevate.lift.strategies.algorithmic.{mapFirstFission, mapFullFission}
import strategies.traversal._
import elevate.lift.strategies.normalForm._
import elevate.lift.strategies.traversal._


class fission_fusion extends test_util.Tests {
  val norm = normalize(betaReduction <+ etaReduction)

  def eq(a: Expr, b: Expr): Unit = {
    if (norm(a).get != norm(b).get) {
      throw new Exception(s"expected structural equality:\n$a\n$b")
    }
  }

  def check(a: Expr, fis: Strategy[Lift], b: Expr, fus: Strategy[Lift]): Unit = {
    val na = norm(a).get
    val nb = norm(b).get
    eq(fis(na).get, nb)
    eq(fus(nb).get, na)
  }

  test("last fission, chain of 2") {
    check(
      fun(f1 => fun(f2 => map(f1 >> f2))),
      position(2)(mapLastFission),
      fun(f1 => fun(f2 => map(f1) >> map(f2))),
      oncetd(mapFusion))
  }

  test("last fission, chain of 3") {
    check(
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2 >> f3)))),
      position(3)(mapLastFission),
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2) >> map(f3)))),
      oncetd(mapFusion))
  }

  test("first fission, chain of 2") {
    check(
      fun(f1 => fun(f2 => map(f1 >> f2))),
      position(2)(mapFirstFission),
      fun(f1 => fun(f2 => map(f1) >> map(f2))),
      oncetd(mapFusion))
  }

  test("first fission, chain of 3") {
    check(
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2 >> f3)))),
      position(3)(mapFirstFission),
      fun(f1 => fun(f2 => fun(f3 => map(f1) >> map(f2 >> f3)))),
      oncetd(mapFusion))
  }

  test("full fission, chain of 2") {
    check(
      fun(f1 => fun(f2 => map(f1 >> f2))),
      position(2)(mapFullFission),
      fun(f1 => fun(f2 => map(f1) >> map(f2))),
      oncetd(mapFusion))
  }

  test("full fission, chain of 3") {
    check(
      fun(f1 => fun(f2 => fun(f3 => map(f1 >> f2 >> f3)))),
      position(3)(mapFullFission),
      fun(f1 => fun(f2 => fun(f3 => map(f1) >> map(f2) >> map(f3)))),
      normalize(mapFusion))
  }
}
