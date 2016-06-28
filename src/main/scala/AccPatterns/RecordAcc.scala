package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

import scala.xml.Elem

case class RecordAcc(fst: Phrase[AccType],
                     snd: Phrase[AccType])
  extends AccPattern {

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

  def toOpenCL(ocl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr], dt: DataType): VarRef = ???

  override def prettyPrint: String =
    s"(${PrettyPrinter(fst)}, ${PrettyPrinter(snd)})"

  override def xmlPrinter: Elem =
    <recordAcc>
      <fst>{Core.xmlPrinter(fst)}</fst>
      <snd>{Core.xmlPrinter(snd)}</snd>
    </recordAcc>
}
