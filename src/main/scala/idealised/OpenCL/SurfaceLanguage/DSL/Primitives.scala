package idealised.OpenCL.SurfaceLanguage.DSL

import idealised.DPIA.Phrases.Literal
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types.{DataType, ExpType, VectorType, float}
import idealised.DPIA._
import idealised.OpenCL.SurfaceLanguage.Primitives._
import idealised.SurfaceLanguage.DSL.{DataExpr, λ}
import idealised.SurfaceLanguage.Expr

object mapGlobal {
  def apply(f: Expr[ExpType -> ExpType]): Expr[ExpType -> ExpType] =
    λ(x => mapGlobal(f, x))

  def apply(f: Expr[ExpType -> ExpType], x: DataExpr) =
    MapGlobal(f, x)
}

object mapSeq {
  def apply(f: Expr[ExpType -> ExpType]): Expr[ExpType -> ExpType] =
    λ(x => mapSeq(f, x))

  def apply(f: Expr[ExpType -> ExpType], x: DataExpr) =
    MapSeq(f, x)
}

object mapWorkgroup {
  def apply(f: Expr[ExpType -> ExpType]): Expr[ExpType -> ExpType] =
    λ(x => mapWorkgroup(f, x))

  def apply(f: Expr[ExpType -> ExpType], x: DataExpr) =
    MapWorkGroup(f, x)
}

object mapLocal {
  def apply(f: Expr[ExpType -> ExpType]): Expr[ExpType -> ExpType] =
    λ(x => mapLocal(f, x))

  def apply(f: Expr[ExpType -> ExpType], x: DataExpr) =
    MapLocal(f, x)
}

object toLocal {
  def apply(f: Expr[ExpType -> ExpType]): Expr[ExpType -> ExpType] =
    λ(x => toLocal(f, x))

  def apply(f: Expr[ExpType -> ExpType], x: DataExpr): ToLocal =
    ToLocal(f, x)
}

object toGlobal {
  def apply(f: Expr[ExpType -> ExpType]): Expr[ExpType -> ExpType] =
    λ(x => toGlobal(f, x))

  def apply(f: Expr[ExpType -> ExpType], x: DataExpr): ToGlobal =
    ToGlobal(f, x)
}

object asVector {
  def apply(n: Nat): Expr[ExpType -> ExpType] =
    λ(array => asVector(n, array))

  def apply(n: Nat, array: DataExpr): AsVector =
    AsVector(n, array)
}

object asScalar {
  def apply(): Expr[ExpType -> ExpType] = λ(array => asScalar(array))

  def apply(array: DataExpr): AsScalar =
    AsScalar(array)
}

object reduceSeq {
  def apply(f: Expr[ExpType -> (ExpType -> ExpType)]): Expr[ExpType -> (ExpType -> ExpType)] =
    λ((init, array) => reduceSeq(f, init, array))

  def apply(f: Expr[ExpType -> (ExpType -> ExpType)], init: Expr[ExpType]): Expr[ExpType -> ExpType] =
    λ(array => reduceSeq(f, init, array))

  def apply(f: Expr[ExpType -> (ExpType -> ExpType)],
            init: DataExpr,
            array: DataExpr) =
    ReduceSeq(f, init, array)
}

object vectorize {
  def apply(len: Int, f: Float) =
    Literal(VectorData(Vector.fill(len)(FloatData(f))), VectorType(len, float))

  def apply(len: Nat, e: DataExpr) = VectorFromScalar(len, e)
}

object oclFun {
  def apply(name: String, inT: DataType, outT: DataType, arg: DataExpr) =
    OpenCLFunction(name, Seq(inT), outT, Seq(arg))

  def apply(name: String, inTs: Seq[DataType], outT: DataType, args: Seq[DataExpr]) =
    OpenCLFunction(name, inTs, outT, args)
}
