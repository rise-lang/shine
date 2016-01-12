package Core

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

  implicit class PairTypeConstructor[T1 <: PhraseType](t1: T1) {
    def x[T2 <: PhraseType](t2: T2) = PairType(t1, t2)
  }

  implicit class FunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def ->[T2 <: PhraseType](t2: T2) = FunctionType(t1, t2)
  }

  implicit class PassiveFunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def `->p`[T2 <: PhraseType](t2: T2) = PassiveFunctionType(t1, t2)
  }
}

// TODO: introduce "type variables"
