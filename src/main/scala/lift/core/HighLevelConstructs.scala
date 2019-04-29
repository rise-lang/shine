package lift.core

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._

object HighLevelConstructs {
  val slide2D: Expr = nFun(sz => nFun(st => fun(a =>
    a |> map(slide(sz)(st)) |> slide(sz)(st) |> map(transpose)
  )))

  val reorderWithStride: Expr = {
    nFun(s => {
      val f =
        implN(n =>
          fun(IndexType(n))(i => i /* FIXME {
            mapIndexExpr(i, j => (j / (n /^ s)) + s * (j % (n /^ s)))
          }*/))
      reorder(f)(f)
    })
  }

  /* TODO?
  val padClamp: Expr = nFun(l => nFun(r =>
    padIdx(l)(r)(fun(i => fun(n =>
      select(i < NatExpr(0))(NatExpr(0))(
        select(i < n)(i)(n - NatExpr(1)))
    )))
  ))
   */

  val padClamp2D: Expr = {
    nFun(b => map(padClamp(b)(b)) >> padClamp(b)(b))
  }
}
