package idealised.OpenCL.SurfaceLanguage.DSL

import idealised.OpenCL.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.DSL.{DataExpr, λ}
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.Semantics._

object mapGlobal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    λ(x => mapGlobal(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr) =
    MapGlobal(f, x)
}

object mapSeq {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    λ(x => mapSeq(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr) =
    MapSeq(f, x)
}

object mapWorkgroup {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    λ(x => mapWorkgroup(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr) =
    MapWorkGroup(f, x)
}

object mapLocal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    λ(x => mapLocal(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr) =
    MapLocal(f, x)
}

object toLocal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    λ(x => toLocal(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): ToLocal =
    ToLocal(f, x)
}

object toGlobal {
  def apply(f: Expr[DataType -> DataType]): Expr[DataType -> DataType] =
    λ(x => toGlobal(f, x))

  def apply(f: Expr[DataType -> DataType], x: DataExpr): ToGlobal =
    ToGlobal(f, x)
}

object asVector {
  def apply(n: Nat): Expr[DataType -> DataType] =
    λ(array => asVector(n, array))

  def apply(n: Nat, array: DataExpr): AsVector =
    AsVector(n, array)
}

object asScalar {
  def apply(): Expr[DataType -> DataType] = λ(array => asScalar(array))

  def apply(array: DataExpr): AsScalar =
    AsScalar(array)
}

object reduceSeq {
  def apply(f: Expr[DataType -> (DataType -> DataType)]): Expr[DataType -> (DataType -> DataType)] =
    λ((init, array) => reduceSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)], init: Expr[DataType]): Expr[DataType -> DataType] =
    λ(array => reduceSeq(f, init, array))

  def apply(f: Expr[DataType -> (DataType -> DataType)],
            init: DataExpr,
            array: DataExpr) =
    ReduceSeq(f, init, array)
}

object vectorize {
  def apply(len: Int, f: Float) =
    LiteralExpr(VectorData(Vector.fill(len)(FloatData(f))), VectorType(len, float))

  def apply(len: Nat, e: DataExpr) = VectorFromScalar(len, e)
}

object oclFun {
  def apply(name: String, inT: DataType, outT: DataType, arg: DataExpr) =
    OpenCLFunction(name, Seq(inT), outT, Seq(arg))

  def apply(name: String, inTs: Seq[DataType], outT: DataType, args: Seq[DataExpr]) =
    OpenCLFunction(name, inTs, outT, args)
}
