package ExpPatterns

import AccPatterns.AsVectorAcc
import Compiling.RewriteToImperative
import Core.OperationalSemantics._
import Core.PhraseType.->
import Core._
import DSL._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

case class AsVector(n: ArithExpr,
                    array: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(m, dt)) if dt.isInstanceOf[BasicType] =>
        ExpType(ArrayType(m /^ n, VectorType(n, dt.asInstanceOf[BasicType])))
      case x => error(x.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    AsVector(n, VisitAndRebuild(array, f))
  }

  override def eval(s: Store): Data = ???

  override def toOpenCL(ocl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = {

    val top = arrayAccess.head
    val newAAS = ((top._1 * n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 /^ n))

    ToOpenCL.exp(array, ocl, newAAS, tupleAccess)
  }

  override def prettyPrint: String = s"(asVector ${n.toString} ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    RewriteToImperative.acc(array)(AsVectorAcc(A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    RewriteToImperative.exp(array)(λ(array.t) { x =>
      C(AsVector(n, x))
    })
  }
}