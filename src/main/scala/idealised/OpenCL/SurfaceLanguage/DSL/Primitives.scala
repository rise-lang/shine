package idealised.OpenCL.SurfaceLanguage.DSL

import idealised.OpenCL.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.DSL.{DataExpr, dFun, fun}
import idealised.SurfaceLanguage.Primitives.DepMap
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

object depMap {
  def apply(f: Expr[DataType -> DataType]):Expr[DataType -> DataType] = fun(x => depMap(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): DepMap = DepMap(dFun(_ => f), x)
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

object asVector {
  def apply(n: Nat): Expr[DataType -> DataType] =
    fun(array => asVector(n, array))

  def apply(n: Nat, array: DataExpr): AsVector =
    AsVector(n, array)
}

object asScalar {
  def apply(): Expr[DataType -> DataType] = fun(array => asScalar(array))

  def apply(array: DataExpr): AsScalar =
    AsScalar(array)
}

object reduceSeq {
  def apply(f: Expr[DataType -> (DataType -> DataType)]): Expr[DataType -> (DataType -> DataType)] =
    fun((init, array) => reduceSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)], init: Expr[DataType]): Expr[DataType -> DataType] =
    fun(array => reduceSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr,
            array: DataExpr) =
    ReduceSeq(f, init, array)
}

object scanSeq {
  def apply(f: Expr[DataType -> (DataType -> DataType)]): Expr[DataType -> (DataType -> DataType)] =
    fun((init, array) => scanSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)], init: Expr[DataType]): Expr[DataType -> DataType] =
    fun(array => scanSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr,
            array: DataExpr) =
    ScanSeq(f, init, array)
}

object vectorize {
  def apply(len: Int, f: Float) =
    LiteralExpr(VectorData(Vector.fill(len)(FloatData(f))))

  def apply(len: Nat, e: DataExpr) = VectorFromScalar(len, e)
}

object oclFun {
  def apply(name: String, inT: DataType, outT: DataType, arg: DataExpr): OpenCLFunction =
    OpenCLFunction(name, Seq(inT), outT, Seq(arg))

  def apply(name: String, inTs: Seq[DataType], outT: DataType, args: Seq[DataExpr]): OpenCLFunction =
    OpenCLFunction(name, inTs, outT, args)
}
