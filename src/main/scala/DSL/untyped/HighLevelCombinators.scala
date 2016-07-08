package DSL.untyped

import Core.OperationalSemantics._
import Core._
import HighLevelCombinators._
import OpenCL.HighLevelCombinators._
import apart.arithmetic.ArithExpr

object map {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] =
    λ(x => map(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]) =
    Map(null, null, null, f, x)
}

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

object zip {
  def apply(lhs: Phrase[ExpType], rhs: Phrase[ExpType]) =
    Zip(null, null, null, lhs, rhs)
}

object split {
  def apply(n: ArithExpr): Phrase[ExpType -> ExpType] =
    λ(array => split(n, array))

  def apply(n: ArithExpr, array: Phrase[ExpType]): Split =
    Split(n, null, null, array)
}

object join {
  def apply(): Phrase[ExpType -> ExpType] = λ(array => join(array))

  def apply(array: Phrase[ExpType]): Join = Join(null, null, null, array)
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

object reduce {
  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)]): Phrase[(ExpType x ExpType) -> ExpType] =
    λ((init, array) => reduce(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType]): Phrase[ExpType -> ExpType] =
    λ(array => reduce(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType],
            array: Phrase[ExpType]): Reduce = {
    Reduce(null, null, null, f, init, array)
  }
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

object iterate {
  def apply(k: ArithExpr, f: Phrase[`(nat)->`[ExpType -> ExpType]]): Phrase[ExpType -> ExpType] =
    λ(array => iterate(k, f, array))

  def apply(k: ArithExpr,
            f: Phrase[`(nat)->`[ExpType -> ExpType]],
            array: Phrase[ExpType]): Iterate =
    Iterate(null, null, k, null, f, array)
}

object gather {
  def apply(idxF: (ArithExpr, DataType) => ArithExpr) = λ(array =>
    Gather(null, null, idxF, array)
  )
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

