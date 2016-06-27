package AccPatterns

import Core._
import Core.OperationalSemantics._
import Core.PrettyPrinter.Indent
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

case class RecordAcc(fst: Phrase[AccType], snd: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    AccType(RecordType( TypeChecker(fst).dataType, TypeChecker(snd).dataType ))
  }

  override def eval(s: Store): AccIdentifier = {
    RecordIdentiers(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    RecordAcc(VisitAndRebuild(fst, fun), VisitAndRebuild(snd, fun))
  }

  override def toOpenCL(ocl: ToOpenCL): VarRef = ???

  def toOpenCL(ocl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = ???

  override def prettyPrint(indent: Indent): String =
    indent + s"(\n" +
      s"${PrettyPrinter(fst, indent.more)},\n" +
      s"${PrettyPrinter(snd, indent.more)}\n" +
      indent + s")"

}
