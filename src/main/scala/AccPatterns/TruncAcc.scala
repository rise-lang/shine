package AccPatterns

import Core._
import Core.OperationalSemantics._
import Core.VisitAndRebuild.fun
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

case class TruncAcc(n: ArithExpr,
                    m: ArithExpr,
                    dt: DataType,
                    array: Phrase[AccType])
  extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(nm, dt_))
        if nm == n+m && dt_ == dt =>
        AccType(ArrayType(n, dt))
      case x => error(x.toString, "ArrayType")
    }
  }

  override def eval(s: Store): AccIdentifier = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    TruncAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def toOpenCL(opencl: ToOpenCL): VarRef = ???

  override def toOpenCL(opencl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = ???

  override def prettyPrint: String = ???
}
