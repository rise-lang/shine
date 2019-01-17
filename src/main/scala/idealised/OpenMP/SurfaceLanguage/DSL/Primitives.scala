package idealised.OpenMP.SurfaceLanguage.DSL

import idealised.OpenMP.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.DSL.{DataExpr, dFun, fun}
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._

object mapPar {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = fun(x => MapPar(f, x))
  def apply(f: Expr[DataType -> DataType], x: DataExpr): MapPar = MapPar(f, x)
}

object mapSeq {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = fun(x => MapSeq(f, x))
  def apply(f: Expr[DataType -> DataType], x: DataExpr): MapSeq = MapSeq(f, x)
}

object depMapPar {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = fun(x => depMapPar(f, x))
  def apply(f: Expr[DataType -> DataType], x:DataExpr): DepMapPar = DepMapPar(dFun(_ => f), x)
}

object reducePar {
  def apply(f: Expr[DataType -> (DataType -> DataType)]): Expr[DataType -> (DataType -> DataType)] =
    fun((init, array) => reducePar(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)], init: Expr[DataType]): Expr[DataType -> DataType] =
    fun(array => reducePar(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)], init: DataExpr, array: DataExpr) =
    ReducePar(f, init, array)
}

object reduceSeq {
  def apply(f: Expr[DataType -> (DataType -> DataType)]): Expr[DataType -> (DataType -> DataType)] =
    fun((init, array) => reduceSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)], init: Expr[DataType]): Expr[DataType -> DataType] =
    fun(array => reduceSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)], init: DataExpr, array: DataExpr) =
    ReduceSeq(f, init, array)
}