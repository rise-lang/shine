package Core

import PhraseType._

sealed abstract class Phrase[T <: PhraseType] {
  var t: T = null.asInstanceOf[T]
}

final case class Ident[T <: PhraseType](name: String)
  extends Phrase[T]

final case class Lambda[T1 <: PhraseType, T2 <: PhraseType](param: Ident[T1], body: Phrase[T2])
  extends Phrase[T1 -> T2]

final case class Apply[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 -> T2], arg: Phrase[T1])
  extends Phrase[T2]

final case class Pair[T1 <: PhraseType, T2 <: PhraseType](fst: Phrase[T1], snd: Phrase[T2])
  extends Phrase[T1 x T2]

final case class Proj1[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T1]

final case class Proj2[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2])
  extends Phrase[T2]

final case class Record(fields: Phrase[ExpType]*) extends Phrase[ExpType]

final case class FieldAccess(n: Int, record: Phrase[ExpType]) extends Phrase[ExpType]

final case class LengthPhrase[T <: BasePhraseTypes](array: Phrase[T]) extends Phrase[ExpType]

final case class ArrayExpAccessPhrase(array: Phrase[ExpType], index: Phrase[ExpType]) extends Phrase[ExpType]

final case class ArrayAccAccessPhrase(array: Phrase[AccType], index: Phrase[ExpType]) extends Phrase[AccType]

case class SkipPhrase()
  extends Phrase[CommandType]

final case class Seq(c1: Phrase[CommandType], c2: Phrase[CommandType])
  extends Phrase[CommandType]

final case class NewPhrase(f: Phrase[(ExpType x AccType) -> CommandType])
  extends Phrase[CommandType]

final case class Assign(lhs: Phrase[AccType], rhs: Phrase[ExpType])
  extends Phrase[CommandType]

final case class IfThenElse[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T])
  extends Phrase[T]

final case class ForPhrase(n: Phrase[ExpType], body: Phrase[ExpType -> CommandType])
  extends Phrase[CommandType]

final case class Literal(d: OperationalSemantics.Data) extends Phrase[ExpType]

final case class BinOp(op: BinOp.Op.Value, lhs: Phrase[ExpType], rhs: Phrase[ExpType])
  extends Phrase[ExpType]

object BinOp {

  object Op extends Enumeration {
    val ADD = Value("+")
    val SUB = Value("-")
    val MUL = Value("*")
    val DIV = Value("/")
    val MOD = Value("%")
  }

}

final case class PatternPhrase(pattern: Pattern) extends Phrase[ExpType]