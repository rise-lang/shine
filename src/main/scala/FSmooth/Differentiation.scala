package FSmooth

import DSL._
import FSmooth.MSmooth._

object Differentiation {
  def deriv(e: Expr, x: Identifier): Expr = {
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
  def fvs(e: Expr): Seq[Identifier] = ???

  def A0(v: Identifier, x: Identifier): Expr = ???

  def A1(v: Identifier, x: Identifier, r: Expr): Expr = ???

  def A2(v: Identifier, x: Identifier, r: Expr, c: Expr): Expr = ???
}
