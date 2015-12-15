import PhraseType._

sealed abstract class Phrase[T <: PhraseType] {
  var t : T = null.asInstanceOf[T]
}

case class Ident[T <: PhraseType](name : String)
  extends Phrase[T]

case class Lambda[T1 <: PhraseType, T2 <: PhraseType](param : Phrase[T1], body : Phrase[T2])
  extends Phrase[T1 -> T2]

case class Apply[T1 <: PhraseType, T2 <: PhraseType](fun : Phrase[T1 -> T2], arg: Phrase[T1])
  extends Phrase[T2]

case class Pair[T1 <: PhraseType, T2 <: PhraseType](fst : Phrase[T1], snd : Phrase[T2])
  extends Phrase[T1 x T2]

case class Proj1[T1 <: PhraseType, T2 <: PhraseType](pair : Phrase[T1 x T2])
  extends Phrase[T1]

case class Proj2[T1 <: PhraseType, T2 <: PhraseType](pair : Phrase[T1 x T2])
  extends Phrase[T2]

case class FieldAccess(n: Int, record: Phrase[ExpType]) extends Phrase[ExpType]

case class SkipPhrase()
  extends Phrase[CommandType]

case class Seq(c1 : Phrase[CommandType], c2 : Phrase[CommandType])
  extends Phrase[CommandType]

case class New(f : Phrase[ (ExpType x AccType) -> CommandType ])
  extends Phrase[CommandType]

case class Assign(lhs : Phrase[AccType], rhs : Phrase[ExpType])
  extends Phrase[CommandType]

case class IfThenElse[T <: PhraseType](cond : Phrase[ExpType], thenP : Phrase[T], elseP : Phrase[T])
  extends Phrase[T]

case class For(n : Phrase[ExpType], body: Phrase[ExpType -> CommandType])
  extends Phrase[CommandType]

case class IntLiteral(i : Int)
  extends Phrase[ExpType]

case class BinOp(op : BinOp.Op.Value, lhs : Phrase[ExpType], rhs : Phrase[ExpType])
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

case class Map(f: Phrase[ExpType -> ExpType], in: Phrase[ExpType]) extends Phrase[ExpType]

case class Zip(lhs: Phrase[ExpType], rhs: Phrase[ExpType]) extends Phrase[ExpType]
