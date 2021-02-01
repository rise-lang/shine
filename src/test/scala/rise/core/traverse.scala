package rise.core

import rise.core.DSL._
import rise.core.Traverse.Traversal
import rise.core.primitives._
import rise.core.traversal._
import rise.core.types._

import scala.collection.mutable

class traverse extends test_util.Tests {
  val e: ToBeTyped[DepLambda[NatKind]] = depFun((h: Nat) =>
    depFun((w: Nat) =>
      fun(ArrayType(h, ArrayType(w, f32)))(input => map(map(fun(x => x)))(input)
      )
    )
  )

  class TraceVisitor(var trace: mutable.ArrayBuffer[Any]) extends Traversal {
    override def typeIdentifier[I <: Kind.Identifier] : I => I = i => {
      trace += i
      super.typeIdentifier(i)
    }

    override def identifier[I <: Identifier] : I => I = i => {
      trace += i
      super.identifier(i)
    }

    override def expr : Expr => Expr = e => {
      trace += e
      super.expr(e)
    }

/*
    override def nat : Nat => Nat = n => {
      trace += n
      super.nat(n)
    }
 */

    override def etype[T <: Type] : T => T = t => {
      trace += t
      super.etype(t)
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
        { case _: App                                    => () },
        { case _: App                                    => () },
        { case primitives.map()                          => () },
        { case _: FunType[_, _]                          => () },
        { case _: FunType[_, _]                          => () },
        { case ArrayType(_, `f32`)                       => () },
        { case ArrayType(_, `f32`)                       => () },
        { case _: FunType[_, _]                          => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case _: App                                    => () },
        { case primitives.map()                          => () },
        { case _: FunType[_, _]                          => () },
        { case _: FunType[_, _]                          => () },
        { case `f32`                                     => () },
        { case `f32`                                     => () },
        { case _: FunType[_, _]                          => () },
        { case ArrayType(_, `f32`)                       => () },
        { case ArrayType(_, `f32`)                       => () },
        { case _: Lambda                                 => () },
        { case _: Identifier                             => () },
        { case _: Identifier                             => () },
        { case _: Identifier                             => () },
        { case _: FunType[_, _]                          => () },
        { case `f32`                                     => () },
        { case `f32`                                     => () },
        { case _: FunType[_, _]                          => () },
        { case ArrayType(_, `f32`)                       => () },
        { case ArrayType(_, `f32`)                       => () },
        { case _: FunType[_, _]                          => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case _: Identifier                             => () },
        { case _: Identifier                             => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case _: FunType[_, _]                          => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case _: DepFunType[NatKind, _] @unchecked      => () },
        { case _: Kind.Identifier                        => () },
        { case _: FunType[_, _]                          => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case _: DepFunType[NatKind, _] @unchecked      => () },
        { case _: Kind.Identifier                        => () },
        { case _: DepFunType[NatKind, _] @unchecked      => () },
        { case _: Kind.Identifier                        => () },
        { case _: FunType[_, _]                          => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
        { case ArrayType(_, ArrayType(_, `f32`))         => () },
      ): Seq[Any => Unit]
    }

    val trace = mutable.ArrayBuffer[Any]()
    val result = Traverse(e, new TraceVisitor(trace))

    // the expression should not have changed
    assert(result == e.toExpr)
    // the trace should match expectations
    trace.length shouldBe expected.length
    trace.zip(expected).foreach({ case (x, e) => e(x) })
  }

  test("traverse an expression depth-first with stop and update") {
    val expected = {
      Seq(
        { case _: DepLambda[NatKind] @unchecked          => () },
        { case _: NatIdentifier                          => () },
        { case _: DepLambda[NatKind] @unchecked          => () },
        { case _: NatIdentifier                          => () },
        { case _: Lambda                                 => () },
        { case _: Identifier                             => () },
        { case _: FunType[_, _]                          => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () },
        { case _: DepFunType[NatKind, _] @unchecked      => () },
        { case _: Kind.Identifier                        => () },
        { case _: FunType[_, _]                          => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () },
        { case _: DepFunType[NatKind, _] @unchecked      => () },
        { case _: Kind.Identifier                        => () },
        { case _: DepFunType[NatKind, _] @unchecked      => () },
        { case _: Kind.Identifier                        => () },
        { case _: FunType[_, _]                          => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () },
      ): Seq[Any => Unit]
    }

    val trace = mutable.ArrayBuffer[Any]()
    class Visitor extends TraceVisitor(trace) {
      override def expr : Expr => Expr = {
        case App(App(primitives.map(), _), e) => app(fun(x => x), preserveType(e))
        case e => super.expr(e)
      }
    }

    val result = Traverse(e, new Visitor)

    // the expression should have changed
    assert(result ==
            depFun((h: Nat) =>
              depFun((w: Nat) =>
                fun(ArrayType(h, ArrayType(w, f32)))(input =>
                  app(fun(x => x), input)
                )
              )
            ).toExpr)
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

    class Visitor extends Traversal {
      override def expr : Expr => Expr = {
          case App(primitives.map(), f) => expr(f)
          case e => super.expr(e)
      }
    }

    val result = Traverse(e, new Visitor)

    // the expression should have changed
    val expected = depFun((n: Nat) =>
      fun(ArrayType(n, f32))(input => {
        val x = identifier(freshName("x"))
        app(lambda(x, x), input |> map(fun(x => x)))
      })
    )
    // TODO: establish proper equality checking
    // assert(r == expected.toExpr)
    assert(result.toUntypedExpr == expected.toUntypedExpr)
  }
}
