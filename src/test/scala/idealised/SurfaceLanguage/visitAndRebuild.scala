package idealised.SurfaceLanguage

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import VisitAndRebuild.{Continue, Result, Stop}

import scala.collection.mutable

class visitAndRebuild extends idealised.util.Tests {
  val e = nFun(h => nFun(w => fun(ArrayType(h, ArrayType(w, float)))(input =>
    map(map(fun(x => x)), input)
  )))

  class TraceVisitor(var trace: mutable.ArrayBuffer[Any]) extends VisitAndRebuild.Visitor {
    override def apply(e: Expr): Result[Expr] = {
      println(e)
      trace += e
      Continue(e, this)
    }

    override def apply(ae: Nat): Nat = {
      println(ae)
      trace += ae
      ae
    }

    override def apply[T <: Type](t: T): T = {
      println(t)
      trace += t
      t
    }
  }

  test("traverse an expression depth-first") {
    val expected = {
      import Primitives._
      Seq(
        { case _: NatDependentLambdaExpr => () },
        { case _: NatIdentifier => () },
        { case _: NatDependentLambdaExpr => () },
        { case _: NatIdentifier => () },
        { case _: LambdaExpr => () },
        { case _: IdentifierExpr => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () },
        { case _: Map => () },
        { case _: LambdaExpr => () },
        { case _: IdentifierExpr => () },
        { case _: Map => () },
        { case _: LambdaExpr => () },
        { case _: IdentifierExpr => () },
        { case _: IdentifierExpr => () },
        { case _: IdentifierExpr => () },
        { case _: IdentifierExpr => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () }
      ) : Seq[Any => Unit]
    }

    val trace = mutable.ArrayBuffer[Any]()
    val result = VisitAndRebuild.DFS(e, new TraceVisitor(trace))

    // the expression should not have changed
    result.value shouldBe e
    // the trace should match expectations
    trace.length shouldBe expected.length
    trace.zip(expected).foreach({ case (x, e) => e(x) })
  }

  test("traverse an expression depth-first with types") {
    val expected = {
      import Primitives._
      Seq(
        { case _: NatDependentLambdaExpr => () },
        { case _: NatIdentifier => () },
        { case _: NatDependentLambdaExpr => () },
        { case _: NatIdentifier => () },
        { case _: LambdaExpr => () },
        { case _: IdentifierExpr => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () },
        { case _: Map => () },
        { case _: LambdaExpr => () },
        { case _: IdentifierExpr => () },
        { case ArrayType(_, _: ScalarType) => () },
        { case _: Map => () },
        { case _: LambdaExpr => () },
        { case _: IdentifierExpr => () },
        { case _: ScalarType => () },
        { case _: IdentifierExpr => () },
        { case _: ScalarType => () },
        { case FunctionType(_: ScalarType, _: ScalarType) => () },
        { case _: IdentifierExpr => () },
        { case ArrayType(_, _: ScalarType) => () },
        { case ArrayType(_, _: ScalarType) => () },
        { case FunctionType(ArrayType(_, _: ScalarType), ArrayType(_, _: ScalarType)) => () },
        { case _: IdentifierExpr => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () },
        { case FunctionType(ArrayType(_, ArrayType(_, _)), ArrayType(_, ArrayType(_, _))) => () },
        { case _: DependentFunctionType[_, _] => () },
        { case _: DependentFunctionType[_, _] => () }
      ) : Seq[Any => Unit]
    }

    val trace = mutable.ArrayBuffer[Any]()
    val typed_e = TypeInference(e, Map())
    val result = VisitAndRebuild.DFS(typed_e, new TraceVisitor(trace))

    // the expression should not have changed
    result.value shouldBe typed_e
    // the trace should match expectations
    trace.length shouldBe expected.length
    trace.zip(expected).foreach({ case (x, e) => e(x) })
  }

  test("traverse an expression depth-first with stop and update") {
    val expected = {
      Seq(
        { case _: NatDependentLambdaExpr => () },
        { case _: NatIdentifier => () },
        { case _: NatDependentLambdaExpr => () },
        { case _: NatIdentifier => () },
        { case _: LambdaExpr => () },
        { case _: IdentifierExpr => () },
        { case ArrayType(_, ArrayType(_, _: ScalarType)) => () }
      ) : Seq[Any => Unit]
    }

    val trace = mutable.ArrayBuffer[Any]()
    class Visitor extends TraceVisitor(trace) {
      override def apply(e: Expr): Result[Expr] = {
        e match {
          case _: Primitives.Map => Stop(fun(x => x))
          case _ => super.apply(e)
        }
      }
    }

    val result = VisitAndRebuild.DFS(e, new Visitor)

    /* TODO: structural equality, the expression should have changed
    result.value shouldBe
      nFun(h => nFun(w => fun(ArrayType(h, ArrayType(w, float)))(input =>
        fun(x => x)
      )))
    */
    // the trace should match expectations
    trace.length shouldBe expected.length
    trace.zip(expected).foreach({ case (x, e) => e(x) })
  }

}
