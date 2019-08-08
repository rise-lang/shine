package FSmooth

import DSL._
import FSmooth.MSmooth._
import FSmooth.traversal._

import scala.collection.mutable

object Differentiation {
  def deriv(e: Expr, x: Variable): Expr = {
    val vi = fvs(e)
    x.t match {
      case Double =>
        D( fun(vi -> e) ).applySeq( vi.map(A0(_, x)) )
      case Array(Double) =>
        build(len(x), fun(r =>
          D( fun(vi -> e) ).applySeq( vi.map(A1(_, x, r))  ) ))
      case Array(Array(Double)) =>
        build(matrixRows(x), fun(r =>
          build(matrixCols(x), fun(c =>
            D( fun(vi -> e) ).applySeq( vi.map(A2(_, x, r, c)) ) )) ))
      case t => throw new Exception(s"Differentiation for type $t unsupported")
    }
  }

  def D(e: Expr): Expr = ???

  // free variables
  def fvs(expr: Expr): Seq[Variable] = {
    val free = mutable.Buffer[Variable]()
    case class Visitor(bound: Set[Variable]) extends traversal.Visitor {
      override def apply(e: Expr): traversal.Result[Expr] = {
        e match {
          case Abstraction(params, _, _) =>
            Continue(e, Visitor(bound ++ params))
          case Let(x, value, body, _) =>
            apply(value) // visit value
            traversal.DepthFirstLocalResult(body, Visitor(bound ++ Set(x))) // visit body
            Stop(e)
          case v: Variable =>
            if (!bound.contains(v)) { free += v }
            Stop(v)
          case _ => Continue(e, this)
        }
      }
    }
    traversal.DepthFirstLocalResult(expr, Visitor(Set()))
    free
  }

  def A0(v: Variable, x: Variable): Expr = {
    if (v == x) pair(x, scalar(1)) else pair(v, scalar(0))
  }

  def A1(v: Variable, x: Variable, r: Expr): Expr = {
    if (v == x) {
      vectorZip(x, vectorHot(len(x), r))
    } else {
      vectorZip(v, vectorFill(len(v), scalar(0.0)))
    }
  }

  def A2(v: Variable, x: Variable, r: Expr, c: Expr): Expr = {
    if (v == x) {
      matrixZip(x, matrixHot(matrixRows(x), matrixCols(x), r, c))
    } else {
      matrixZip(v, matrixZeros(matrixRows(v), matrixCols(v)))
    }
  }
}
