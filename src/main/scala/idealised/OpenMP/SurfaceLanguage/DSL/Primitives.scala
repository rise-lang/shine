package idealised.OpenMP.SurfaceLanguage.DSL

import idealised.OpenMP.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.DSL.{fun, nFun}
import idealised.SurfaceLanguage.Expr

object mapPar {
  def apply(f: Expr): Expr = fun(x => MapPar(f, x))
  def apply(f: Expr, x: Expr): MapPar = MapPar(f, x)
}

object depMapPar {
  def apply(f: Expr): Expr = fun(x => depMapPar(f, x))
  def apply(f: Expr, x:Expr): DepMapPar = DepMapPar(nFun(_ => f), x)
}

object reducePar {
  def apply(f: Expr): Expr =
    fun((init, array) => reducePar(f, init, array))

  def apply(f: Expr, init: Expr): Expr =
    fun(array => reducePar(f, init, array))

  def apply(f: Expr, init: Expr, array: Expr) =
    ReducePar(f, init, array)
}
