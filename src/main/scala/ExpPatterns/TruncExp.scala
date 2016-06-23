package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->
import Core.VisitAndRebuild.fun
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.{Expression, VarRef}

case class TruncExp(n: ArithExpr,
                    m: ArithExpr,
                    dt: DataType,
                    array: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n_, dt_)) =>
        if (n_ == n && dt_ == dt) {
          ExpType(ArrayType(m, dt))
        } else {
          error(s"[$n_.$dt_]", s"[$n.$dt]")
        }
      case x => error(x.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Data = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    TruncExp(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???

  override def prettyPrint: String = ???

  override def toOpenCL(ocl: ToOpenCL,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr]): Expression = {
    ToOpenCL.exp(array, ocl, arrayAccess, tupleAccess)
  }

}
