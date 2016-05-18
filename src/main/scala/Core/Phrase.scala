package Core

import PhraseType._

sealed abstract class Phrase[T <: PhraseType] {
  var t: T = null.asInstanceOf[T]
}

final case class IdentPhrase[T <: PhraseType](name: String)
  extends Phrase[T]

final case class LambdaPhrase[T1 <: PhraseType, T2 <: PhraseType](param: IdentPhrase[T1], body: Phrase[T2])
  extends Phrase[T1 -> T2]

final case class ApplyPhrase[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 -> T2], arg: Phrase[T1])
  extends Phrase[T2]

final case class PairPhrase[T1 <: PhraseType, T2 <: PhraseType](fst: Phrase[T1], snd: Phrase[T2])
  extends Phrase[T1 x T2]

final case class Proj1Phrase[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T1]

final case class Proj2Phrase[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T2]

final case class RecordExpPhase(fst: Phrase[ExpType], snd: Phrase[ExpType])
  extends Phrase[ExpType]

final case class FstExprPhrase(record: Phrase[ExpType])
  extends Phrase[ExpType]

final case class SndExprPhrase(record: Phrase[ExpType])
  extends Phrase[ExpType]

final case class RecordAccPhase(fst: Phrase[AccType], snd: Phrase[AccType])
  extends Phrase[AccType]

final case class FstAccPhrase(record: Phrase[AccType])
  extends Phrase[AccType]

final case class SndAccPhrase(record: Phrase[AccType])
  extends Phrase[AccType]

final case class ArrayExpAccessPhrase(array: Phrase[ExpType], index: Phrase[ExpType])
  extends Phrase[ExpType]

final case class ArrayAccAccessPhrase(array: Phrase[AccType], index: Phrase[ExpType])
  extends Phrase[AccType]

final case class LengthPhrase[T <: BasePhraseTypes](array: Phrase[T])
  extends Phrase[ExpType]

case class SkipPhrase()
  extends Phrase[CommandType]

final case class SeqPhrase(c1: Phrase[CommandType], c2: Phrase[CommandType])
  extends Phrase[CommandType]

final case class NewPhrase(f: Phrase[(ExpType x AccType) -> CommandType])
  extends Phrase[CommandType]

final case class AssignPhrase(lhs: Phrase[AccType], rhs: Phrase[ExpType])
  extends Phrase[CommandType]

final case class IfThenElsePhrase[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T])
  extends Phrase[T]

final case class ForPhrase(n: Phrase[ExpType], body: Phrase[ExpType -> CommandType])
  extends Phrase[CommandType]

final case class LiteralPhrase(d: OperationalSemantics.Data)
  extends Phrase[ExpType]

final case class BinOpPhrase(op: BinOpPhrase.Op.Value, lhs: Phrase[ExpType], rhs: Phrase[ExpType])
  extends Phrase[ExpType]

object BinOpPhrase {

  object Op extends Enumeration {
    val ADD = Value("+")
    val SUB = Value("-")
    val MUL = Value("*")
    val DIV = Value("/")
    val MOD = Value("%")
  }

}

final case class ExpPatternPhrase(pattern: ExpPattern)
  extends Phrase[ExpType]

final case class AccPatternPhrase(pattern: AccPattern)
  extends Phrase[AccType]

final case class CommandPatternPhrase(pattern: CommandPattern)
  extends Phrase[CommandType]