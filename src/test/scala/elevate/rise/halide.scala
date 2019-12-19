package elevate.rise

import elevate.core.strategies.traversal._
import elevate.core.{NotApplicable, Strategy}
import elevate.rise.rules.traversal._
import elevate.rise.strategies.halide._
import elevate.rise.strategies.normalForm._
import elevate.util._
import lift.core.TypedDSL.{reorder => _, _}

class halide extends test_util.Tests {
  private def LCNFrewrite(a: Rise, s: Strategy[Rise], b: Rise): Unit = {
    val (closedA, nA) = makeClosed(a)
    val (closedB, nB) = makeClosed(b)
    val na = LCNF(closedA).get
    val nb = LCNF(closedB).get
    assert(position(nA)(s).apply(na).get == nb)
  }

  test("generic reorder 1D") {
    val expr = toExpr(λ(i => λ(f => *!(f) $ i)))
    assert(body(body(reorder(Seq(1))))(expr).get == expr)

    try(body(body(reorder(Seq(1,2))))(expr).get) catch {
      case NotApplicable(_) => assert(true)
      case _:Throwable => assert(false)
    }
  }

  test("generic reorder 2D") {
    val expr = λ(i => λ(f => **!(f) $ i))
    val gold = λ(i => λ(f => (T o **!(f) o T) $ i))
    LCNFrewrite(expr, body(body(reorder(Seq(1,2)))), expr)
    LCNFrewrite(expr, body(body(reorder(Seq(2,1)))), gold)
  }

  test("generic reorder 3D") {
    val expr = λ(i => λ(f => ***!(f) $ i))
    val gold132 = λ(i => λ(f => (*!(T) o ***!(f) o *!(T)) $ i))
    val gold213 = λ(i => λ(f => (T o ***!(f) o T) $ i))
    val gold231 = λ(i => λ(f => (T o *!(T) o ***!(f) o *!(T) o T) $ i))
    val gold321 = λ(i => λ(f => (*!(T) o T o *!(T) o ***!(f) o *!(T) o T o *!(T)) $ i))
    val gold312 = λ(i => λ(f => (*!(T) o T o ***!(f) o T o *!(T)) $ i))

    LCNFrewrite(expr, body(body(reorder(Seq(1,2,3)))), expr)
    LCNFrewrite(expr, body(body(reorder(Seq(1,3,2)))), gold132)
    LCNFrewrite(expr, body(body(reorder(Seq(2,1,3)))), gold213)
    LCNFrewrite(expr, body(body(reorder(Seq(2,3,1)))), gold231)
    LCNFrewrite(expr, body(body(reorder(Seq(3,2,1)))), gold321)
    LCNFrewrite(expr, body(body(reorder(Seq(3,1,2)))), gold312)
  }

  test("generic reorder 4D") {
    val expr = λ(i => λ(f => ****!(f) $ i))
    val gold1243 = λ(i => λ(f => (**!(T) o ****!(f) o **!(T)) $ i))
    val gold1324 = λ(i => λ(f => (*!(T) o ****!(f) o *!(T)) $ i))
    val gold2134 = λ(i => λ(f => (T o ****!(f) o T) $ i))
    val gold4321 = λ(i => λ(f => (**!(T) o *!(T) o T o **!(T) o *!(T) o **!(T) o ****!(f) o
      **!(T) o *!(T) o **!(T) o T o  *!(T) o **!(T) ) $ i))
    // just trying a few here
    LCNFrewrite(expr, body(body(reorder(Seq(1,2,3,4)))), expr)
    LCNFrewrite(expr, body(body(reorder(Seq(1,2,4,3)))), gold1243)
    LCNFrewrite(expr, body(body(reorder(Seq(1,3,2,4)))), gold1324)
    LCNFrewrite(expr, body(body(reorder(Seq(2,1,3,4)))), gold2134)
    LCNFrewrite(expr, body(body(reorder(Seq(4,3,2,1)))), gold4321)
  }

}
