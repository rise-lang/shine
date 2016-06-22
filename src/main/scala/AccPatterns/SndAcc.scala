package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

case class SndAcc(dt1: DataType,
                  dt2: DataType,
                  record: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(record) match {
      case AccType(RecordType(fst, snd)) if fst == dt1 && snd == dt2 =>
        AccType(snd)
      case t => error(t.toString, "Something else")
    }
  }

  override def eval(s: Store): AccIdentifier = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordIdentiers => r.snd
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[AccType] = {
    SndAcc(dt1, dt2, VisitAndRebuild(record, f))
  }

  override def toOpenCL(ocl: ToOpenCL): VarRef = ???

  def toOpenCL(ocl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = ???

  override def prettyPrint: String = s"${PrettyPrinter(record)}._2"

}
