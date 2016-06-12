package Core

import PhraseType._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.{Block, Expression, VarRef}

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

final case class IfThenElsePhrase[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T], elseP: Phrase[T])
  extends Phrase[T]

final case class UnaryOpPhrase(op: UnaryOpPhrase.Op.Value, p: Phrase[ExpType])
  extends Phrase[ExpType]

object UnaryOpPhrase {

  object Op extends Enumeration {
    val NEG = Value("-")
  }

}

final case class BinOpPhrase(op: BinOpPhrase.Op.Value, lhs: Phrase[ExpType], rhs: Phrase[ExpType])
  extends Phrase[ExpType]

object BinOpPhrase {

  object Op extends Enumeration {
    val ADD = Value("+")
    val SUB = Value("-")
    val MUL = Value("*")
    val DIV = Value("/")
    val MOD = Value("%")
    val GT = Value(">")
    val LT = Value("<")
  }

}

final case class LiteralPhrase(d: OperationalSemantics.Data)
  extends Phrase[ExpType]

abstract class ExpPattern extends Phrase[ExpType] {
  def typeCheck(): ExpType

  def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data

  def toOpenCL: Expression

  def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression

  def prettyPrint: String

  def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType]

  def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType]
}

abstract class AccPattern extends Phrase[AccType] {
  def typeCheck(): AccType

  def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): AccPattern

  def eval(s: OperationalSemantics.Store): OperationalSemantics.AccIdentifier

  def toOpenCL: VarRef

  def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef

  def prettyPrint: String
}

abstract class CommandPattern extends  Phrase[CommandType] {
  def typeCheck(): CommandType

  def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Store

  def toOpenCL(block: Block): Block

  def prettyPrint: String

  def substituteImpl: Phrase[CommandType]
}