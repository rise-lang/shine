package idealised.OpenCL.SurfaceLanguage.DSL

import idealised.OpenCL.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.DSL.{DataExpr, dFun, fun}
import idealised.SurfaceLanguage.Primitives.{AsScalar, AsVector, DepMapSeq}
import idealised.SurfaceLanguage.Semantics._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, _}

import scala.language.reflectiveCalls

object mapGlobal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = mapGlobal(0)(f)
  def apply(f: Expr[DataType -> DataType], x: DataExpr): MapGlobal = mapGlobal(0)(f, x)

  def apply(dim: Int) = new {
    def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
      fun(x => MapGlobal(dim)(f, x))

    def apply(f: Expr[DataType -> DataType], x: DataExpr): MapGlobal =
      MapGlobal(dim)(f, x)
  }
}

object depMapGlobal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = depMapGlobal(0)(f)
  def apply(f: Expr[DataType -> DataType], x: DataExpr): DepMapGlobal = depMapGlobal(0)(f, x)


  def apply(dim: Int) = new {
    def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
      fun(x => DepMapGlobal(dim)(dFun(_ => f), x))

    def apply(f: Expr[DataType -> DataType], x: DataExpr): DepMapGlobal =
      DepMapGlobal(dim)(dFun(_ => f), x)
  }
}

object mapWorkgroup {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = mapWorkgroup(0)(f)
  def apply(f: Expr[DataType -> DataType], x: DataExpr): MapWorkGroup = mapWorkgroup(0)(f, x)

  def apply(dim: Int) = new {
    def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
      fun(x => MapWorkGroup(dim)(f, x))

    def apply(f: Expr[DataType -> DataType], x: DataExpr): MapWorkGroup =
      MapWorkGroup(dim)(f, x)
  }
}

object mapLocal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] = mapLocal(0)(f)
  def apply(f: Expr[DataType -> DataType], x: DataExpr): MapLocal = mapLocal(0)(f, x)

  def apply(dim: Int) = new {
    def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
      fun(x => MapLocal(dim)(f, x))

    def apply(f: Expr[DataType -> DataType], x: DataExpr): MapLocal =
      MapLocal(dim)(f, x)
  }
}

object toLocal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    fun(x => toLocal(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): ToLocal =
    ToLocal(f, x)
}

object toGlobal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    fun(x => toGlobal(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): ToGlobal =
    ToGlobal(f, x)
}

object toPrivate {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    fun(x => toPrivate(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): ToPrivate =
    ToPrivate(f, x)
}

object oclFun {
  def apply(name: String, inT: DataType, outT: DataType, arg: DataExpr): OpenCLFunction =
    OpenCLFunction(name, Seq(inT), outT, Seq(arg))

  def apply(name: String, inTs: Seq[DataType], outT: DataType, args: Seq[DataExpr]): OpenCLFunction =
    OpenCLFunction(name, inTs, outT, args)
}
