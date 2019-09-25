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

  def D(e: Expr): Expr = e match {
    case Application(VectorFunctionConstants.build(_), Seq(e0, e1), _) =>
      build( fst(D(e0)), fun(i => D(e1) (pair(i, scalar(0)))) )
    case Application(VectorFunctionConstants.ifold(_), Seq(e0, e1, e2), _) =>
      ifold( fun( (x, i) => D(e0) (x, pair(i, scalar(0)))), D(e1), fst(D(e2)) )
    case Application(VectorFunctionConstants.get(_), Seq(e0, e1), _) =>
      D(e0).get(fst(D(e1)))
    case Application(VectorFunctionConstants.length(_), Seq(e0), _) =>
      pair(len(D(e0)), scalar(0))
    case Application(PairFunctionConstants.pair(_), Seq(e0, e1), _) =>
      pair(D(e0), D(e1))
    case Application(PairFunctionConstants.fst(_), Seq(e0), _) =>
      fst(D(e0))
    case Application(PairFunctionConstants.snd(_), Seq(e0), _) =>
      snd(D(e0))
    case Application(e0, Seq(e1), _)  => D(e0) (D(e1))
    case Abstraction(Seq(x), e, _)    => fun(Seq(mark(x)) -> D(e))
    case v: Variable                  => mark(v)
    case Let(x, e1, e2, _)            => Let(mark(x), D(e1), D(e2))
    case Conditional(e1, e2, e3, _)   =>`if` (fst(D(e1))) `then` D(e2) `else` D(e3)
    case ScalarValue(e)               => pair(P(e), E(e))
  }

  def P(e: Double) = ???
  def E(e: Double) = ???

  def mark(v: Variable): Variable = {
    v.copy(v.name + "_")
  }

  // free variables
  def fvs(expr: Expr): Seq[Variable] = {
    val free = mutable.Buffer[Variable]()

    def visit(e: Expr, bound: Set[Variable]): Unit = e match {
      case Abstraction(params, body, _) =>
        visit(body, bound ++ params)
      case Application(fun, args, _) =>
        visit(fun, bound)
        args.foreach(visit(_, bound))
      case Let(x, value, body, _) =>
        visit(value, bound)
        visit(body, bound + x)
      case Conditional(c, tr, el, _) =>
        visit(c, bound)
        visit(tr, bound)
        visit(el, bound)
      case v: Variable =>
        if (!bound.contains(v)) { free += v }
      case ScalarValue(_) | IndexValue(_) |
           CardinalityValue(_) | _: Constants =>
    }

    visit(expr, Set())
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
