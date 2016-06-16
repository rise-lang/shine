package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

case class Record(fst: Phrase[ExpType], snd: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    ExpType(RecordType( TypeChecker(fst).dataType, TypeChecker(snd).dataType ))
  }

  override def eval(s: Store): Data = {
    RecordData(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    Record(VisitAndRebuild(fst, f), VisitAndRebuild(snd, f))
  }

  override def toOpenCL: Expression = ???

  override def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = ???

  override def prettyPrint: String = s"(${PrettyPrinter(fst)}, ${PrettyPrinter(snd)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
