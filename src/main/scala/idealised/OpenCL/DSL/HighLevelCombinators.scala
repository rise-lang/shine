package idealised.OpenCL.DSL

import idealised.Core.OperationalSemantics._
import idealised.Core._
import idealised.DSL.untyped.λ
import idealised.OpenCL.HighLevelCombinators._
import idealised.OpenCL.LowLevelCombinators.VectorFromScalar

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
  def apply(n: Nat): Phrase[ExpType -> ExpType] =
    λ(array => asVector(n, array))

  def apply(n: Nat, array: Phrase[ExpType]): AsVector =
    AsVector(n, null, null, array)
}

object asScalar {
  def apply(): Phrase[ExpType -> ExpType] = λ(array => asScalar(array))

  def apply(array: Phrase[ExpType]): AsScalar =
    AsScalar(null, null, null, array)
}

object reduceSeq {
  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)]): Phrase[(ExpType x ExpType) -> ExpType] =
    λ((init, array) => reduceSeq(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType]): Phrase[ExpType -> ExpType] =
    λ(array => reduceSeq(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)],
            init: Phrase[ExpType],
            array: Phrase[ExpType]) =
    ReduceSeq(null, null, null, f, init, array)
}

object vectorize {
  def apply(len: Int, f: Float) =
    LiteralPhrase(VectorData(Vector.fill(len)(FloatData(f))), VectorType(len, float))

  def apply(len: Nat, e: Phrase[ExpType]) = VectorFromScalar(len, null, e)
}
