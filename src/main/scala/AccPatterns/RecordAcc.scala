package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

import scala.xml.Elem

case class RecordAcc(fst: Phrase[AccType],
                     snd: Phrase[AccType])
  extends AccPattern {

  override lazy val `type` = acc"[${fst.t.dataType} x ${snd.t.dataType}]"

  override def typeCheck: Unit = { }

  override def eval(s: Store): AccIdentifier = {
    RecordIdentiers(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    RecordAcc(VisitAndRebuild(fst, fun), VisitAndRebuild(snd, fun))
  }

  override def toOpenCL(env: ToOpenCL.Environment): VarRef = ???

  def toOpenCL(env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr], dt: DataType): VarRef = ???

  override def prettyPrint: String =
    s"(${PrettyPrinter(fst)}, ${PrettyPrinter(snd)})"

  override def xmlPrinter: Elem =
    <recordAcc>
      <fst>{Core.xmlPrinter(fst)}</fst>
      <snd>{Core.xmlPrinter(snd)}</snd>
    </recordAcc>
}
