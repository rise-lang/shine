package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import DSL._
import Rewriting.RewriteToImperative
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.{Expression, Literal}

case class Snd(record: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    TypeChecker(record) match {
      case ExpType(RecordType(fst, snd)) => ExpType(snd)
      case t => TypeChecker.error(t.toString, "Something else")
    }
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordData => r.snd
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    Snd(VisitAndRebuild(record, f))
  }

  override def toOpenCL: Expression = ToOpenCL.exp(this, List(), List())

  override def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = {
    ToOpenCL.exp(record, arrayAccess, 2 :: tupleAccess)
  }

  override def prettyPrint: String = s"${PrettyPrinter(record)}._2"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] =
    RewriteToImperative.exp(this, λ(this.t) {
      this.t.dataType match {
        case _: BasicType => x => A `:=` x
        case _: ArrayType => throw new Exception("This should not happen")
        case _: RecordType => throw new Exception("This should not happen")
      }
    })

  override def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = C(this)
}
