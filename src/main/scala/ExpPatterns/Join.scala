package ExpPatterns

import AccPatterns.JoinAcc
import Core.OperationalSemantics._
import Core.PhraseType.->
import Core._
import Rewriting.RewriteToImperative
import DSL._
import apart.arithmetic.ArithExpr
import ir.Type
import opencl.generator.OpenCLAST.Expression

case class Join(array: Phrase[ExpType]) extends ExpPattern {

  private var n: ArithExpr = null

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n_, ArrayType(m, dt))) =>
        n = n_
        ExpType(ArrayType(n*m, dt))
      case x => error(x.toString, "ArrayType(ArrayType)")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    Join(VisitAndRebuild(array, f))
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, array) match {
      case ArrayData(outer) =>
        val arrays = outer.map {
          case ArrayData(inner) => inner
          case _ => throw new Exception("This should not happen")
        }
        ArrayData(arrays.flatten)

      case _ => throw new Exception("This should not happen")
    }
  }

  override def toOpenCL(ocl: ToOpenCL): Expression = ???

  override def toOpenCL(ocl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): Expression = {
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val chunkId = idx._1 / n
    val chunkElemId = idx._1 % n

    val l = Type.getLengths( DataType.toType(t.dataType) ).reduce(_*_)

    val newAs = (chunkId, l * n) :: (chunkElemId, l) :: stack

    ToOpenCL.exp(array, ocl, newAs, tupleAccess)
  }

  override def prettyPrint: String = s"(join ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    assert(n != null)
    RewriteToImperative.acc(array, JoinAcc(n, A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    RewriteToImperative.exp(array, λ(array.t) { x =>
      C(Join(x))
    })
  }
}