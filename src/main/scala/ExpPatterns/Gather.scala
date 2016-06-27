package ExpPatterns

import Core.OperationalSemantics._
import Core.PhraseType._
import Core.VisitAndRebuild.fun
import Core._
import Compiling.RewriteToImperative
import Core.PrettyPrinter.Indent
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression
import DSL._

case class Gather(idxF: (ArithExpr, DataType) => ArithExpr,
                  array: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern {

  override def typeCheck(): ExpType = TypeChecker(array)

  override def eval(s: Store): Data = {
    import OperationalSemantics._
    OperationalSemantics.eval(s, array) match {
      case ArrayData(a) =>
        val res = Array[Data](a.length)
        for (i <- a.indices) {
          res(i) =  a( idxF(i, array.t.dataType).eval )
        }
        ArrayData(res.toVector)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(f: fun): Phrase[ExpType] =
    Gather(idxF, VisitAndRebuild(array, f))

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(this)(λ(array.t) { x =>
      acc(x)(A)
    })
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(array)(λ(array.t) { x =>
      C(Gather(idxF, x))
    })
  }

  override def toOpenCL(ocl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = {

    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val newIdx = idxF(idx._1, array.t.dataType)

    ToOpenCL.exp(array, ocl, (newIdx, idx._2) :: stack, tupleAccess)

  }

  override def prettyPrint(indent: Indent): String = indent + s"(gather idxF ${PrettyPrinter(array)})"
}
