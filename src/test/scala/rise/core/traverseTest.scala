package rise.core

import rise.core.DSL._
import rise.core.traverse._
import rise.core.primitives._
import rise.core.types._

class traverseTest extends test_util.Tests {
  val e: ToBeTyped[DepLambda[NatKind]] = depFun((h: Nat) =>
    depFun((w: Nat) =>
      fun(ArrayType(h, ArrayType(w, f32)))(input => map(map(fun(x => x)))(input)
      )
    )
  )

  case class Trace[T](unwrap : T) { val trace : Seq[Any] = Seq() }
  implicit object TraceMonad extends Monad[Trace] {
    def write[T] : T => Trace[T] = t => new Trace(t) { override val trace : Seq[Any] = Seq(t)}
    override def return_[T] : T => Trace[T] = t => Trace(t)
    override def bind[T,S] : Trace[T] => (T => Trace[S]) => Trace[S] = t1 => f => {
      val t2 = f(t1.unwrap)
      new Trace(t2.unwrap) { override val trace : Seq[Any] = t1.trace ++ t2.trace}
    }
  }

  class TraceVisitor extends Traversal[Trace] {
    override def monad = TraceMonad

    override def typeIdentifier[I <: Kind.Identifier] : VarType => I => Trace[I] = vt => i =>
      monad.bind(monad.write(i))(super.typeIdentifier(vt)(_))

    override def identifier[I <: Identifier] : VarType => I => Trace[I] = vt => i =>
      monad.bind(monad.write(i))(super.identifier(vt)(_))

    override def expr : Expr => Trace[Expr] = e =>
      monad.bind(monad.write(e))(super.expr)

    override def `type`[T <: Type] : T => Trace[T] = t =>
    monad.bind(monad.write(t))(super.`type`)
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
        { case `f32`                                     => () },
        { case _: Identifier                             => () },
        { case _: Identifier                             => () },
        { case `f32`                                     => () },
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

    val result = traverse(e, new TraceVisitor())

    // the expression should not have changed
    assert(result.unwrap == e.toExpr)
    // the trace should match expectations
    result.trace.length shouldBe expected.length
    result.trace.zip(expected).foreach({ case (x, e) => e(x) })
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
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () },
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

    class Visitor extends TraceVisitor {
      override def expr : Expr => Trace[Expr] = {
        case App(App(primitives.map(), _), e) => monad.return_(app(fun(x => x), preserveType(e)))
        case e => super.expr(e)
      }
    }

    val result = traverse(e, new Visitor)

    // the expression should have changed
    assert(result.unwrap ==
            depFun((h: Nat) =>
              depFun((w: Nat) =>
                fun(ArrayType(h, ArrayType(w, f32)))(input =>
                  app(fun(x => x), input)
                )
              )
            ).toExpr)
    // the trace should match expectations
    result.trace.length shouldBe expected.length
    result.trace.zip(expected).foreach({ case (x, e) => e(x) })
  }

  test("traverse an expression depth-first with global stop") {
    val e = depFun((n: Nat) =>
      fun(ArrayType(n, f32))(input =>
        input |> map(fun(x => x)) |> map(fun(x => x))
      )
    )

    class Visitor extends PureTraversal {
      override def expr : Expr => Pure[Expr] = {
          case App(App(primitives.map(), _), e) => monad.return_(app(fun(x => x), preserveType(e)))
          case e => super.expr(e)
      }
    }

    val result = traverse(e, new Visitor)

    // the expression should have changed
    val expected = depFun((n: Nat) =>
      fun(ArrayType(n, f32))(input => {
        val x = identifier(freshName("x"))
        input |> map(fun(x => x)) |> fun(x => x)
      })
    )
    // TODO: establish proper equality checking
    // assert(r == expected.toExpr)
    assert(result.toUntypedExpr == expected.toUntypedExpr)
  }
}
