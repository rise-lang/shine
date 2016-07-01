package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

import scala.xml.Elem

case class SndAcc(dt1: DataType,
                  dt2: DataType,
                  record: Phrase[AccType]) extends AccPattern {

  override lazy val `type` = acc"[$dt2]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    record checkType acc"[$dt1 x $dt2]"
  }

  override def eval(s: Store): AccIdentifier = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordIdentiers => r.snd
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    SndAcc(fun(dt1), fun(dt2), VisitAndRebuild(record, fun))
  }

  override def toOpenCL(env: ToOpenCL.Environment): VarRef = ???

  def toOpenCL(env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr], dt: DataType): VarRef = ???

  override def prettyPrint: String = s"(SndAcc ${PrettyPrinter(record)})"

  override def xmlPrinter: Elem =
    <sndAcc dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Core.xmlPrinter(record)}
    </sndAcc>
}
