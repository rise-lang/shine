package HighLevelCombinators

import Compiling.RewriteToImperative
import Core.OperationalSemantics._
import Core._
import DSL._
import LowLevelCombinators.JoinAcc
import apart.arithmetic.ArithExpr
import ir.Type
import opencl.generator.OpenCLAST.Expression

import scala.xml.Elem

case class Join(n: ArithExpr,
                m: ArithExpr,
                dt: DataType,
                array: Phrase[ExpType])
  extends HighLevelCombinator with ViewExp {

  override lazy val `type` = exp"[${n * m}.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    array checkType exp"[$n.$m.$dt]"
  }

  override def inferTypes: Join = {
    import TypeInference._
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(n_, ArrayType(m_, dt_))) => Join(n_, m_, dt_, array_)
      case x => error(x.toString, "ExpType(ArrayType(ArrayType))")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    Join(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
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

  override def toOpenCL(env: ToOpenCL.Environment,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): Expression = {
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val chunkId = idx._1 / n
    val chunkElemId = idx._1 % n

    val l = Type.getLengths(DataType.toType(t.dataType)).reduce(_ * _)

    val newAs = (chunkId, l * n) ::(chunkElemId, l) :: stack

    ToOpenCL.exp(array, env, newAs, tupleAccess, dt)
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
      C(Join(n, m, dt, x))
    })
  }
}