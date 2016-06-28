package ExpPatterns

import AccPatterns.JoinAcc
import Core.OperationalSemantics._
import Core.PhraseType.->
import Core._
import Compiling.RewriteToImperative
import DSL._
import apart.arithmetic.ArithExpr
import ir.Type
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

case class Join(array: Phrase[ExpType])
  extends ExpPattern with ViewExpPattern {

  private var n: ArithExpr = null
  private var m: ArithExpr = null
  private var dt: DataType = null

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n_, ArrayType(m_, dt_))) =>
        n = n_; m = m_; dt = dt_
        ExpType(ArrayType(n*m, dt))
      case x => error(x.toString, "ArrayType(ArrayType)")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    val j = Join(VisitAndRebuild(array, fun))
    j.n = fun(n)
    j.m = fun(m)
    j.dt = fun(dt)
    j
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

  override def toOpenCL(ocl: ToOpenCL,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val chunkId = idx._1 / n
    val chunkElemId = idx._1 % n

    val l = Type.getLengths( DataType.toType(t.dataType) ).reduce(_*_)

    val newAs = (chunkId, l * n) :: (chunkElemId, l) :: stack

    ToOpenCL.exp(array, ocl, newAs, tupleAccess, dt)
  }

  override def prettyPrint: String = s"(join ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <join n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Core.xmlPrinter(array)}
    </join>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    assert(n != null && m != null && dt != null)
    import RewriteToImperative._
    acc(array)(JoinAcc(n, m, dt, A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(array)(λ(array.t) { x =>
      C(Join(x))
    })
  }
}