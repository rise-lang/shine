package OpenCL.DSL

import Core.OperationalSemantics._
import Core._
import DSL.untyped.λ
import OpenCL.HighLevelCombinators._
import apart.arithmetic.ArithExpr

object mapGlobal {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] =
    λ(x => mapGlobal(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]) =
    MapGlobal(null, null, null, f, x)
}

object mapSeq {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] =
    λ(x => mapSeq(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]) =
    MapSeq(null, null, null, f, x)
}

object mapWorkgroup {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] =
    λ(x => mapWorkgroup(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]) =
    MapWorkGroup(null, null, null, f, x)
}

object mapLocal {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] =
    λ(x => mapLocal(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]) =
    MapLocal(null, null, null, f, x)
}

object toLocal {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] =
    λ(x => toLocal(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]): ToLocal =
    ToLocal(null, null, f, x)
}

object toGlobal {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] =
    λ(x => toGlobal(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]): ToGlobal =
    ToGlobal(null, null, f, x)
}

object asVector {
  def apply(n: ArithExpr): Phrase[ExpType -> ExpType] =
    λ(array => asVector(n, array))

  def apply(n: ArithExpr, array: Phrase[ExpType]): AsVector =
    AsVector(n, null, null, array)
}

object asScalar {
  def apply(): Phrase[ExpType -> ExpType] = λ(array => asScalar(array))

  def apply(array: Phrase[ExpType]): AsScalar =
    AsScalar(null, null, null, array)
}

object vectorize {
  def apply(len: Int, f: Float) =
    LiteralPhrase(VectorData(Vector.fill(len)(FloatData(f))))
}
