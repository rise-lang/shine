package lift.core

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._

object HighLevelConstructs {
  val slide2D = nFun(sz => nFun(st => fun(a =>
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
}
