sealed trait PhraseType

abstract class BasePhraseTypes extends PhraseType

case class ExpType(dataType: DataType) extends BasePhraseTypes

case class AccType(dataType: DataType) extends BasePhraseTypes

case class CommandType() extends PhraseType

case class PairType[T1 <: PhraseType, T2 <: PhraseType](t1: T1, t2: T2) extends PhraseType

case class FunctionType[T1 <: PhraseType, T2 <: PhraseType](inT: T1, outT: T2) extends PhraseType

case class PassiveFunctionType[T1 <: PhraseType, T2 <: PhraseType](inT: T1, outT: T2)
  extends PhraseType

// convenience types for writing the phrase types more readable
object PhraseType {
  // TODO: move into package object ...

  type x[T1 <: PhraseType, T2 <: PhraseType] = PairType[T1, T2]
  type ->[T1 <: PhraseType, T2 <: PhraseType] = FunctionType[T1, T2]
  type `->p`[T1 <: PhraseType, T2 <: PhraseType] = PassiveFunctionType[T1, T2]
  type VarType = ExpType x AccType
}

// TODO: introduce "type variables"
