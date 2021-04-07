package rise.core

import rise.core.DSL._
import rise.core.traverse._
import rise.core.primitives._
import rise.core.types._

class traverseTest extends test_util.Tests {
  val e: Expr = depFun((h: Nat) =>
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

  class ExprTraceVisitor extends Traversal[Trace] {
    override def monad = TraceMonad

    override def identifier[I <: Identifier] : VarType => I => Trace[I] = vt => i =>
      monad.bind(monad.write(i))(super.identifier(vt)(_))
  }

  class TypeTraceVisitor extends Traversal[Trace] {
    override def monad = TraceMonad

    override def typeIdentifier[I <: Kind.Identifier] : VarType => I => Trace[I] = vt => i =>
      monad.bind(monad.write(i))(super.typeIdentifier(vt)(_))
  }

  def checkEqualities[T] : Seq[T] => Seq[Seq[Int]] => Boolean = xs => gps =>
    gps.forall(gp => gp.map(xs(_)).distinct.length == 1)

  test("traversing an expression should traverse identifiers in order") {
    val equivs = Seq(Seq(0, 3), Seq(1, 2))
    val result = traverse(e, new ExprTraceVisitor())

    // the expression should not have changed
    assert(result.unwrap == e)
    // the trace should match expectations
    result.trace.length shouldBe equivs.flatten.length
    assert(checkEqualities(result.trace)(equivs))
  }

  test("traversing a type should traverse identifiers in order") {
    val equivs = Seq(Seq(0, 2, 4), Seq(1, 3, 5))
    val result = traverse(e.t, new TypeTraceVisitor())
    // the type should not have changed
    assert(result.unwrap == e.t)
    // the trace should match expectations
    result.trace.length shouldBe equivs.flatten.length
    assert(checkEqualities(result.trace)(equivs))
  }

  test("traverse an expression depth-first with stop and update") {
    class Visitor extends PureExprTraversal {
      override def expr : Expr => Pure[Expr] = {
        case App(App(primitives.map(), _), e) => monad.return_(app(fun(x => x), preserveType(e)))
        case e => super.expr(e)
      }
    }

    val result = traverse(e, new Visitor)

    // the expression should have changed
    assert(result =~~=
            depFun((h: Nat) =>
              depFun((w: Nat) =>
                fun(ArrayType(h, ArrayType(w, f32)))(input =>
                  app(fun(x => x), input)
                )
              )
            ).toExpr)
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
    assert(result =~~= expected)
  }
}
