package AccPatterns

import Core._
import Core.OperationalSemantics._
import Core.PrettyPrinter.Indent
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
      case AccType(ArrayType(n_, dt_)) =>
        if (n_ == n && dt_ == dt) {
          AccType(ArrayType(m, dt))
        } else {
          error(s"[$n_.$dt_]", s"[$n.$dt]")
        }
      case x => error(x.toString, "ArrayType")
    }
  }

  override def eval(s: Store): AccIdentifier = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    TruncAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def toOpenCL(opencl: ToOpenCL): VarRef = ???

  override def toOpenCL(ocl: ToOpenCL,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr]): VarRef = {
    ToOpenCL.acc(array, ocl, arrayAccess, tupleAccess)
  }

  override def prettyPrint(indent: Indent): String =
    indent + s"(truncAcc\n" +
      s"${PrettyPrinter(array, indent.more)}\n" +
      indent + s")"
}
