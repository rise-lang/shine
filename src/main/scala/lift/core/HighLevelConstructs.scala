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
          fun(IndexType(n).`_`(R))(i => i /* FIXME {
            mapIndexExpr(i, j => (j / (n /^ s)) + s * (j % (n /^ s)))
          }*/))
      reorder(f)(f)
    })
  }

  val padClamp2D: Expr = {
    nFun(b => map(padClamp(b)(b)) >> padClamp(b)(b))
  }
}
