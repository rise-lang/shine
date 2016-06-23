package Core

import PhraseType._
import apart.arithmetic.{ArithExpr, NamedVar}
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

final case class NatDependentLambdaPhrase[T <: PhraseType](x: NamedVar, body: Phrase[T])
  extends Phrase[`(nat)->`[T]]

final case class NatDependentApplyPhrase[T <: PhraseType](fun: Phrase[`(nat)->`[T]], arg: ArithExpr)
  extends Phrase[T]

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

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data

  def prettyPrint: String

  def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType]

  def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType]

  def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType]
}

trait GeneratableExpPattern {
  def toOpenCL(opencl: ToOpenCL): Expression
}

trait ViewExpPattern {
  def toOpenCL(opencl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression
}

abstract class AccPattern extends Phrase[AccType] {
  def typeCheck(): AccType

  def eval(s: OperationalSemantics.Store): OperationalSemantics.AccIdentifier

  def toOpenCL(opencl: ToOpenCL): VarRef

  def toOpenCL(opencl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef

  def prettyPrint: String

  def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[AccType]
}

abstract class IntermediateCommandPattern extends  Phrase[CommandType] {
  def typeCheck(): CommandType

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Store

  def prettyPrint: String

  def substituteImpl: Phrase[CommandType]

  def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[CommandType]
}

abstract class CommandPattern extends IntermediateCommandPattern {
  def toOpenCL(block: Block, ocl: ToOpenCL): Block
}
