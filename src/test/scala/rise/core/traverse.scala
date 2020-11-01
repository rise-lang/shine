package rise.core

import rise.core.dsl._
import rise.core.exprs.{App, DepLambda, Expr, Identifier, Lambda, primitives}
import rise.core.exprs.primitives._
import rise.core.util.traversal._
import rise.core.types._
import rise.core.util.{freshName, traversal}

import scala.collection.mutable

class traverse extends test_util.Tests {
  val e: ToBeTyped[DepLambda[NatKind]] = depFun((h: Nat) =>
    depFun((w: Nat) =>
      fun(ArrayType(h, ArrayType(w, f32)))(input => map(map(fun(x => x)))(input)
      )
    )
  )

  class TraceVisitor(var trace: mutable.ArrayBuffer[Any]) extends Visitor {
    override def visitExpr(e: Expr): Result[Expr] = {
      println(e)
      trace += e
      Continue(e, this)
    }

    override def visitNat(ae: Nat): Result[Nat] = {
      println(ae)
      trace += ae
      Continue(ae, this)
    }

    override def visitType[T <: Type](t: T): Result[T] = {
      println(t)
      trace += t
      Continue(t, this)
    }
  }

  test("traverse an expression depth-first") {
    val expected = {
      Seq(
        { case _: DepLambda[NatKind] @unchecked          => () },
        { case _: NatIdentifier                          => () },
        { case _: DepLambda[NatKind] @unchecked          => () },
        { case _: NatIdentifier                          => () },
        { case _: Lambda                                 => () },
        { case _: Identifier                             => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case _: exprs.App                                    => () },
        { case _: exprs.App                                    => () },
        { case primitives.map()                          => () },
        { case _: FunType[_, _]                          => () },
        { case _: exprs.App                                    => () },
        { case primitives.map()                          => () },
        { case _: FunType[_, _]                          => () },
        { case _: Lambda                                 => () },
        { case _: Identifier                             => () },
        { case `f32`                                     => () },
        { case _: Identifier                             => () },
        { case `f32`                                     => () },
        { case _: FunType[_, _]                          => () },
        { case _: FunType[_, _]                          => () },
        { case _: FunType[_, _]                          => () },
        { case _: Identifier                             => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case _: FunType[_, _]                          => () },
        { case _: DepFunType[NatKind, _] @unchecked      => () },
        { case _: DepFunType[NatKind, _] @unchecked      => () }
      ): Seq[Any => Unit]
    }

    val trace = mutable.ArrayBuffer[Any]()
    val result = DepthFirstLocalResult(e, new TraceVisitor(trace))

    // the expression should not have changed
    assert(result == e.toExpr)
    // the trace should match expectations
    trace.length shouldBe expected.length
    trace.zip(expected).foreach({ case (x, e) => e(x) })
  }

  /* TODO?
  test("traverse an expression depth-first with types") {
  }
   */

  test("traverse an expression depth-first with stop and update") {
    val expected = {
      Seq(
        { case _: DepLambda[NatKind] @unchecked          => () },
        { case _: NatIdentifier                          => () },
        { case _: DepLambda[NatKind] @unchecked          => () },
        { case _: NatIdentifier                          => () },
        { case _: Lambda                                 => () },
        { case _: Identifier                             => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () }
      ): Seq[Any => Unit]
    }

    val trace = mutable.ArrayBuffer[Any]()
    class Visitor extends TraceVisitor(trace) {
      override def visitExpr(expr: Expr): Result[Expr] = {
        expr match {
          case App(App(primitives.map(), _), e) =>
            val r = app(fun(x => x), preserveType(e))
            println(r)
            Stop(r)
          case _ => super.visitExpr(expr)
        }
      }
    }

    val result = DepthFirstGlobalResult(e, new Visitor)

    // the expression should have changed
    result match {
      case traversal.Stop(r) =>
        assert(
          r ==
            depFun((h: Nat) =>
              depFun((w: Nat) =>
                fun(ArrayType(h, ArrayType(w, f32)))(input =>
                  app(fun(x => x), input)
                )
              )
            ).toExpr
        )
      case _ => throw new Exception("the traversal should have stopped")
    }
    // the trace should match expectations
    trace.length shouldBe expected.length
    trace.zip(expected).foreach({ case (x, e) => e(x) })
  }

  test("traverse an expression depth-first with global stop") {
    val e = depFun((n: Nat) =>
      fun(ArrayType(n, f32))(input =>
        input |> map(fun(x => x)) |> map(fun(x => x))
      )
    )

    class Visitor extends traversal.Visitor {
      override def visitExpr(expr: Expr): Result[Expr] = {
        expr match {
          case App(primitives.map(), f) =>
            println(f)
            Stop(f)
          case _ => Continue(expr, this)
        }
      }
    }

    val result = DepthFirstGlobalResult(e, new Visitor)

    // the expression should have changed
    (result: @unchecked) match {
      case traversal.Stop(r) =>
        val expected = depFun((n: Nat) =>
          fun(ArrayType(n, f32))(input => {
            val x = identifier(freshName("x"))
            app(lambda(x, x), input |> map(fun(x => x)))
          })
        )
        // TODO: establish proper equality checking
        // assert(r == expected.toExpr)
        assert(r.toUntypedExpr == expected.toUntypedExpr)
    }
  }
}
