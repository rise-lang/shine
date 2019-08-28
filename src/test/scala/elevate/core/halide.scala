package elevate.core

import elevate.core.strategies.basic.{debug, id}
import elevate.lift.strategies.traversal.body
import elevate.lift.strategies.normalForm._
import elevate.lift.strategies.halide._
import elevate.util._
import lift.core.DSL._
import lift.core.types.infer

class halide extends idealised.util.Tests {

  test("generic reorder 1D") {
    val expr = λ(i => λ(f => *!(f) $ i))
    assert(body(body(reorder(Seq(1))))(expr).get == expr)

    try(body(body(reorder(Seq(1,2))))(expr).get) catch {
      case NotApplicable(_) => assert(true)
      case _:Throwable => assert(false)
    }
  }

  test("generic reorder 2D") {
    val expr = λ(i => λ(f => **!(f) $ i))
    val gold = λ(i => λ(f => (T o **!(f) o T) $ i))
    assert(body(body(reorder(Seq(1,2))))(expr).get == expr)
    assert(body(body(reorder(Seq(2,1))))(expr).get == gold)
  }

  test("generic reorder 3D") {
    val expr = λ(i => λ(f => ***!(f) $ i))
    val gold132 = λ(i => λ(f => (*!(T) o ***!(f) o *!(T)) $ i))
    val gold213 = λ(i => λ(f => (T o ***!(f) o T) $ i))
    val gold231 = λ(i => λ(f => (T o *!(T) o ***!(f) o *!(T) o T) $ i))
    val gold321 = λ(i => λ(f => (*!(T) o T o *!(T) o ***!(f) o *!(T) o T o *!(T)) $ i))
    val gold312 = λ(i => λ(f => (*!(T) o T o ***!(f) o T o *!(T)) $ i))

    assert(body(body(reorder(Seq(1,2,3))))(expr).get == expr)
    assert(body(body(reorder(Seq(1,3,2))))(expr).get == gold132)
    assert(body(body(reorder(Seq(2,1,3))))(expr).get == gold213)
    assert(body(body(reorder(Seq(2,3,1))))(expr).get == gold231)
    assert(body(body(reorder(Seq(3,2,1))))(expr).get == gold321)
    assert(body(body(reorder(Seq(3,1,2))))(expr).get == gold312)
  }

  test("generic reorder 4D") {
    val expr = λ(i => λ(f => ****!(f) $ i))
    val gold1243 = λ(i => λ(f => (**!(T) o ****!(f) o **!(T)) $ i))
    val gold1324 = λ(i => λ(f => (*!(T) o ****!(f) o *!(T)) $ i))
    val gold2134 = λ(i => λ(f => (T o ****!(f) o T) $ i))
    val gold4321 = λ(i => λ(f => (**!(T) o *!(T) o T o **!(T) o *!(T) o **!(T) o ****!(f) o
      **!(T) o *!(T) o **!(T) o T o  *!(T) o **!(T) ) $ i))
    // just trying a few here
    assert(body(body(reorder(Seq(1,2,3,4))))(expr).get == expr)
    assert(body(body(reorder(Seq(1,2,4,3))))(expr).get == gold1243)
    assert(body(body(reorder(Seq(1,3,2,4))))(expr).get == gold1324)
    assert(body(body(reorder(Seq(2,1,3,4))))(expr).get == gold2134)
    assert(body(body(reorder(Seq(4,3,2,1))))(expr).get == gold4321)
  }

}
