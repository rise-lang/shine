package shine.DPIA.Types

import rise.core.types.{Access, DataType, Kind}

sealed trait PhraseType

sealed abstract class BasePhraseType(val dataType: DataType) extends PhraseType

final case class ExpType(override val dataType: DataType, accessType: Access) extends BasePhraseType(dataType) {
  override def toString = s"exp[$dataType, $accessType]"
}

final case class AccType(override val dataType: DataType) extends BasePhraseType(dataType) {
  override def toString = s"acc[$dataType]"
}

sealed case class CommType() extends PhraseType {
  override def toString = "comm"
}

object comm extends CommType

final case class PhrasePairType[T1 <: PhraseType, T2 <: PhraseType](t1: T1, t2: T2) extends PhraseType {
  override def toString = s"$t1 x $t2"
}

final case class FunType[T <: PhraseType, +R <: PhraseType](inT: T, outT: R) extends PhraseType {
  override def toString = s"($inT) -> $outT"
}

final case class PassiveFunType[T <: PhraseType, +R <: PhraseType](inT: T, outT: R) extends PhraseType {
  override def toString = s"($inT) ->p $outT"
}

final case class DepFunType[I, +R <: PhraseType](kind: Kind[_, I], x: I, t: R) extends PhraseType {
  override def toString = s"(${Kind.idName(kind, x)}: ${kind.name}) -> $t"
}
